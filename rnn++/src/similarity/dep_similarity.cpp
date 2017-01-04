#include <algorithm>
#include <map>

#include <fmt/printf.h>

#include "data_source/ygp_db.h"
#include "data_source/db.h"
#include "data_source/rss.h"

#include "similarity/dep_similarity.h"
#include "similarity/similarity_measure.h"

#include "utils/parallel.h"
#include "utils/profiling.h"
#include "utils/span.h"
#include "utils/string.h"
#include "utils/algorithm.h"
#include "utils/hdf5.h"
#include "utils/math.h"
#include "utils/linear_algebra.h"
#include "utils/versioned_name.h"

using namespace wordrep;
using namespace util::io;
namespace ygp = data::ygp;

using util::json_t;
using util::Timer;
using data::PerSentQueryResult;
using data::DBIndexer;
using data::ColumnUID;

namespace {
using engine::ScoredSentence;

std::vector<ScoredSentence> deduplicate_results(tbb::concurrent_vector<ScoredSentence> const &relevant_sents){
    using hash_t = size_t;
    std::map<hash_t, bool> is_seen{};
    std::vector<ScoredSentence> dedup_sents;
    for(auto const &scored_sent : relevant_sents){
        auto sent = scored_sent.sent;
        hash_t hash(0);
        for(auto idx=sent.beg; idx!=sent.end; ++idx){
            hash += std::hash<VocaIndex>{}(sent.tokens->word(idx));
        }
        if(is_seen.find(hash)!=is_seen.cend()) continue;
        is_seen[hash]=true;
        dedup_sents.push_back(scored_sent);
    }
    return dedup_sents;
}

}//nameless namespace

namespace engine {

WordSimCache::WordSimCache(voca_info_t const &voca) : voca{voca} {
    auto n= voca.wvecs.size();
    data_t::accessor a;
    distance_caches.insert(a, wordrep::VocaIndex{});
    a->second = dist_cache_t{n};//For unknown word
}

bool WordSimCache::find(wordrep::VocaIndex idx) const{
    data_t::const_accessor a;
    return distance_caches.find(a, idx);
}
bool WordSimCache::insert(wordrep::VocaIndex idx, dist_cache_t const &dist){
    data_t::accessor a;
    distance_caches.find(a, idx);
    if(distance_caches.find(a, idx)) return false;
    distance_caches.insert(a, idx);
    a->second = dist;
    return true;
}
const WordSimCache::dist_cache_t& WordSimCache::distances(wordrep::VocaIndex widx) const {
    data_t::const_accessor a;
    bool is_exist=distance_caches.find(a,widx);
    //TODO:     make cache private method and remove this assert.
    if(!is_exist) assert(0);
    return a->second;
}
void WordSimCache::cache(std::vector<VocaIndex> const &words) {
    auto n= voca.wvecs.size();
    std::vector<VocaIndex> words_to_cache;
    std::vector<dist_cache_t> dists;
    for(auto vidx : words) {
        if(find(vidx)) continue;
        words_to_cache.push_back(vidx);
        dists.push_back(dist_cache_t{n});
    }
    auto n_words = dists.size();

    auto dist_measure = similarity::Similarity<similarity::measure::angle>{};
    tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,10000),
                      [&](tbb::blocked_range<decltype(n)> const &r){
                          for(decltype(n) i=r.begin(); i!=r.end(); ++i){
                              for(decltype(n_words)j=0; j!=n_words; ++j ){
                                  auto qidx = words_to_cache[j];
                                  auto q = voca.wvecs[qidx];
                                  auto widx = VocaIndex::from_unsigned(i);
                                  dists[j][widx] = dist_measure(voca.wvecs[widx], q);
                              }
                          }
                      });

    for(decltype(n_words)i=0; i!=n_words; ++i){
        auto vidx=words_to_cache[i];
        insert(vidx,dists[i]);
    }
}

WordSimCache::val_t WordSimCache::max_similarity(wordrep::VocaIndex widx) const{
    if(!find(widx)) return 0.0;
    auto dists = distances(widx);
    auto beg = dists.val.begin();
    std::partial_sort(beg, beg+2, dists.val.end(), std::greater<val_t>{});
    auto it = beg+1;
    return *it;
}


void QueryResultCache::insert(wordrep::SentUID uid, result_t const&result) {
    data_t::accessor a;
    caches.insert(a, uid);
    a->second = result;
    std::cerr<<fmt::format("Insert {} to a cache", uid.val)<<std::endl;
}
QueryResultCache::result_t QueryResultCache::get(wordrep::SentUID uid) const {
    data_t::const_accessor a;
    if(caches.find(a, uid)) {
        std::cerr<<fmt::format("Cache hits: {}", uid.val)<<std::endl;
        return a->second;
    }
    std::cerr<<fmt::format("Cache misses: {}", uid.val)<<std::endl;
    return result_t{};
}
bool QueryResultCache::find(wordrep::SentUID uid) const {
    data_t::const_accessor a;
    std::cerr<<fmt::format("Look for a cache {} ", uid.val)<<std::endl;
    return caches.find(a, uid);
}

DepSearchScore::val_t DepSearchScore::score_sum() const {return util::math::sum(scores);}

class DepParsedQuery{
public:
    using val_t = WordSimCache::val_t;
    DepParsedQuery(std::vector<val_t> const &cutoffs, Sentence query_sent, WordSimCache const &similarity)
    : len{diff(query_sent.end,query_sent.beg)}, query_sent{query_sent}, cutoffs{cutoffs}, dists{} {
        for(auto idx=query_sent.beg; idx!=query_sent.end; ++idx)
            sorted_idxs.push_back({cutoffs[diff(idx,query_sent.beg)],idx});
        std::sort(sorted_idxs.begin(),sorted_idxs.end(),[](auto x, auto y){return x.first>y.first;});
        val_t sum=0.0;
        std::vector<val_t> cutoff_cumsum;
        for(auto pair : sorted_idxs) {
            sum += pair.first;
            cutoff_cumsum.push_back(sum);
        }
        auto it = std::find_if_not(cutoff_cumsum.cbegin(),cutoff_cumsum.cend(),
                                   [&cutoff_cumsum](auto x){return x/cutoff_cumsum.back()<0.3;});
        auto it2 = std::find_if_not(cutoff_cumsum.cbegin(),cutoff_cumsum.cend(),
                                   [&cutoff_cumsum](auto x){return x/cutoff_cumsum.back()<0.5;});
        auto it3 = std::find_if_not(cutoff_cumsum.cbegin(),cutoff_cumsum.cend(),
                                   [&cutoff_cumsum](auto x){return x/cutoff_cumsum.back()<0.8;});
        n_cut = it - cutoff_cumsum.cbegin();
        n_cut2 = it2 - cutoff_cumsum.cbegin();
        n_cut3 = it3 - cutoff_cumsum.cbegin();
        cut = *it * 0.5;
        cut2 = *it2 * 0.5;
        cut3 = *it3 * 0.5;
        fmt::print("n_cut = {}, {}, {}, cut ={}, {}, {}\n", n_cut, n_cut2, n_cut3, cut, cut2, cut3);

        for(auto idx=query_sent.beg; idx!=query_sent.end; ++idx)
            dists.push_back(&similarity.distances(query_sent.tokens->word(idx)));
    }

    DepSearchScore get_scores(Sentence const &sent) const {
        auto beg=sent.beg;
        auto end=sent.end;
        val_t total_score{0.0};
//        std::vector<std::pair<DPTokenIndex, val_t>>  scores(len);
        DepSearchScore scores(len);
        auto i_trial{0};

        for(auto pair: sorted_idxs){
            ++i_trial;
            DPTokenIndex tidx = pair.second;
            auto j = diff(tidx, query_sent.beg);
            val_t score{0.0};
            if(cutoffs[j]<0.4) continue;
            assert(query_sent.tokens->word_pos(tidx).val==j);
            for(auto i=beg; i!=end; ++i) {
                auto word = sent.tokens->word(i);
                auto dependent_score = (*dists[j])[word];
                auto head_word = sent.tokens->head_word(i);
                auto maybe_qhead_pidx = query_sent.tokens->head_pos(tidx);
                if(!maybe_qhead_pidx) {
                    auto tmp = cutoffs[j] * dependent_score;
                    if(tmp>score){
                        score = tmp;
//                        scores[j] = {i, score};
                        scores.set(j, tidx, i, score);
                    }
                } else {
                    auto qhead_pidx = maybe_qhead_pidx.value().val;
                    //CAUTION: this early stopping assumes tmp =  cutoffs[j] * dependent_score * governor_score*cutoffs[qhead_pidx];
                    // if(cutoffs[qhead_pidx]<0.4) continue;
                    auto governor_score = (*dists[qhead_pidx])[head_word];
                    auto tmp = cutoffs[j] * dependent_score * (1 + governor_score*cutoffs[qhead_pidx]);
                    if(tmp>score){
                        score = tmp;
                        scores.set(j, tidx, i, score);
//                        scores[j] = {i, score};
                    }
                }
            }

            total_score += score;
            if(i_trial==n_cut){
                if(total_score <cut) return scores;
            }
            else if(i_trial==n_cut2){
                if(total_score < cut2) return scores;
            }
            else if(i_trial==n_cut3){
                if(total_score < cut3) return scores;
            }
        }
        return scores;
    }
    std::size_t n_words() const {return len;}

private:
    int64_t len;
    Sentence query_sent;
    std::vector<val_t> cutoffs;
    std::vector<std::pair<val_t,DPTokenIndex>> sorted_idxs; //Descending order of cutoff.
    std::ptrdiff_t n_cut;
    std::ptrdiff_t n_cut2;
    std::ptrdiff_t n_cut3;
    val_t cut;
    val_t cut2;
    val_t cut3;
    std::vector<WordSimCache::dist_cache_t const*> dists;
};


////////////////////////////////

template<typename OPC, typename OPR>
std::vector<data::PerSentQueryResult> write_output(
        Sentence const &query_sent,
        std::vector<ScoredSentence> const &relevant_sents,
        OPC op_cut, OPR op_results) {
    auto n_found = relevant_sents.size();
    std::cerr<<n_found << " results are found"<<std::endl;

    util::Timer timer;
    auto top_N_results = op_cut(relevant_sents);
    timer.here_then_reset("Get top N results.");

    std::vector<data::PerSentQueryResult> results;
    for(auto const &scored_sent : top_N_results){
        auto result = op_results(query_sent, scored_sent);
        results.push_back(result);
    }

    timer.here_then_reset("Generate JSON output.");
    return results;
}

//TODO : separated out helper functions

util::json_t to_json(std::vector<PerSentQueryResult> const &results){
    util::json_t answer{};

    for(auto const &result : results){
        answer["score"].push_back(result.score);
        answer["result_sent_country"].push_back(result.country);
        data::build_db_info_field(answer, result);
        answer["result_table_name"].push_back(result.table_name);
        answer["result_column_name"].push_back(result.column_name);
        answer["result_index_col_name"].push_back(result.index_col_name);
        auto tmp2 = result.highlight_offset;
        answer["highlight_offset"].push_back({tmp2.beg, tmp2.end});
        auto tmp3 = result.clip_offset;
        answer["clip_offset"].push_back({tmp3.beg, tmp3.end});

        util::json_t score_with_offset{};
        for(auto elm :result.scores_with_offset) {
            score_with_offset.push_back({elm.score,
                                         elm.query_word.beg, elm.query_word.end,
                                         elm.matched_word.beg, elm.matched_word.end});
        }
        answer["score_with_offset"].push_back(score_with_offset);
    }
    return answer;
}

void annotate_input_info(util::json_t &answer, data::QuerySentInfo const &info){
    answer["input_offset"]={info.offset.beg,info.offset.end};
    answer["input_uid"] = info.sent_uid;
    answer["cutoffs"] = info.cutoffs;
    answer["words"] = info.words;
}
util::json_t to_json(std::vector<data::QueryResult> const &answers){
    util::json_t output{};
    for(auto &answer : answers){
        auto answer_json = to_json(answer.results);
        answer_json["n_relevant_matches"] = answer.n_relevant_matches;
        annotate_input_info(answer_json, answer.query);
        output.push_back(answer_json);
    }
    return output;
}

data::QuerySentInfo construct_query_info(
        Sentence query_sent, wordrep::WordUIDindex const& wordUIDs,
        wordrep::WordImportance const& word_importance) {
    data::QuerySentInfo info;
    for(auto idx = query_sent.beg; idx!=query_sent.end; ++idx) {
        auto wuid = query_sent.tokens->word_uid(idx);
        auto word = wordUIDs[wuid];
        info.words.push_back(word);
        auto cutoff = word_importance.score(wuid);
        info.cutoffs.push_back(cutoff>1.0?0.0:cutoff);
    }
    info.sent_uid = query_sent.uid.val;
    info.offset.beg = query_sent.tokens->word_beg(query_sent.beg).val;
    info.offset.end = query_sent.tokens->word_end(query_sent.end-1).val;
    return info;
}

data::QuerySentInfo construct_query_info(
        Sentence query_sent, wordrep::WordUIDindex const& wordUIDs,
        wordrep::WordImportance const& word_importance,
        engine::WordSimCache const &dists_cache) {
    data::QuerySentInfo info;
    for(auto idx = query_sent.beg; idx!=query_sent.end; ++idx) {
        auto wuid = query_sent.tokens->word_uid(idx);
        auto word = wordUIDs[wuid];
        info.words.push_back(word);
        auto vidx = query_sent.tokens->word(idx);
        auto cutoff = word_importance.score(wuid);
        auto importance = cutoff>1.0? 0.0:cutoff;
        auto max_sim = dists_cache.max_similarity(vidx);
        if(cutoff<0.0000001) {cutoff = max_sim;}
        else {cutoff =  importance<max_sim? importance : max_sim;}
        info.cutoffs.push_back(cutoff);
    }
    info.sent_uid = query_sent.uid.val;
    info.offset.beg = query_sent.tokens->word_beg(query_sent.beg).val;
    info.offset.end = query_sent.tokens->word_end(query_sent.end-1).val;
    return info;
}

void cache_words(Sentence const &sent, WordSimCache &dists_cache) {
    std::vector<VocaIndex> vidxs;
    for(auto idx = sent.beg; idx!=sent.end; ++idx) {
        auto vuid=sent.tokens->word(idx);
        vidxs.push_back(vuid);
    }
    dists_cache.cache(vidxs);
}
////////////////////////////////////////////////////////////////

struct ProcessQuerySent{
    using val_t = WordSimCache::val_t;
    ProcessQuerySent(WordSimCache &dists_cache)
            : dists_cache{dists_cache}
    {}

    std::vector<ScoredSentence> operator()(Sentence query_sent,
                                           std::vector<val_t> const &cutoffs,
                                           std::vector<Sentence> const &data_sents) {
        DepParsedQuery query{cutoffs, query_sent, dists_cache};

        tbb::concurrent_vector<ScoredSentence> relevant_sents{};
        auto n = data_sents.size();
        tbb::parallel_for(decltype(n){0}, n, [&](auto i) {
            auto sent = data_sents[i];
            auto scores = query.get_scores(sent);
            ScoredSentence scored_sent{sent, scores};
            if (scored_sent.score > util::math::sum(cutoffs) * 0.5){
                relevant_sents.push_back(scored_sent);
            }
        });
        return deduplicate_results(relevant_sents);
    }
    WordSimCache& dists_cache;
};


struct ProcessQuerySents{
    ProcessQuerySents(wordrep::WordUIDindex const& wordUIDs,
                      wordrep::WordImportance const& word_importance,
                      WordSimCache& dists_cache)
            : wordUIDs{wordUIDs}, word_importance{word_importance},
              dists_cache{dists_cache},
              processor{dists_cache}
    {}

    template<typename OP>
    void operator()(std::vector<wordrep::Sentence> const &query_sents,
                    std::vector<wordrep::Sentence> const &candidate_sents,
                    OP const &op_per_sent) {
        util::Timer timer{};
        tbb::task_group g;
        for(auto const &query_sent : query_sents){
            if(query_sent.beg==query_sent.end) continue;
            g.run([&timer,query_sent,&op_per_sent,&candidate_sents, this](){
                data::QuerySentInfo info = construct_query_info(query_sent, wordUIDs, word_importance);
                timer.here_then_reset("Get cutoffs");
                cache_words(query_sent, dists_cache);
                timer.here_then_reset("Built Similarity caches.");
                std::cerr<<fmt::format("Query : Find with {} candidate sentences.",candidate_sents.size())<<std::endl;
                auto relevant_sents = processor(query_sent, info.cutoffs, candidate_sents);
                op_per_sent(query_sent, info, relevant_sents);
                timer.here("Query answered.");
            });
        }
        timer.here_then_reset("All Queries are answered.");
        g.wait();
    }

    wordrep::WordUIDindex const& wordUIDs;
    wordrep::WordImportance const& word_importance;
    WordSimCache& dists_cache;
    ProcessQuerySent processor;
};

struct ProcessChainQuery{
    ProcessChainQuery(wordrep::WordUIDindex const& wordUIDs,
                      wordrep::WordImportance const& word_importance,
                      wordrep::Sentences const &uid2sent,
                      WordSimCache& dists_cache)
            : wordUIDs{wordUIDs}, word_importance{word_importance}, uid2sent{uid2sent},
              dists_cache{dists_cache},
              processor{dists_cache}
    {}

    template<typename OP>
    void operator()(std::vector<wordrep::Sentence> const &query_chain,
                    std::vector<wordrep::Sentence> candidate_sents,
                    OP const &op_per_sent) {
        util::Timer timer{};
        for(auto const &query_sent : query_chain){
            if(query_sent.beg==query_sent.end) continue;
            cache_words(query_sent, dists_cache);
            data::QuerySentInfo info = construct_query_info(query_sent, wordUIDs, word_importance, dists_cache);
            timer.here_then_reset("Get cutoffs");

            auto relevant_sents = processor(query_sent, info.cutoffs, candidate_sents);//util::deserialize<val_t>(info.cutoffs)

            candidate_sents.clear();
            assert(candidate_sents.size()==0);
            if(!relevant_sents.size()) continue;

            op_per_sent(query_sent, info, relevant_sents);
            timer.here_then_reset("One pass in a query chain is finished.");

            auto best_candidate = std::max_element(relevant_sents.cbegin(), relevant_sents.cend(),
                                                   [](auto x, auto y){return x.score<y.score;});
            auto score_cutoff = best_candidate->score * 0.5;
            for(auto scored_sent : relevant_sents){
                if(scored_sent.score < score_cutoff) continue;
                auto sent = scored_sent.sent;
                //TODO: release following assumption that candidate_sents are only from dataset, not queries_sents.
                //TODO: fix inefficienty; collecting all uids first.
                auto uids = sent.tokens->sentences_in_chunk(sent);
                for(auto uid : uids) candidate_sents.push_back(uid2sent[uid]);
                //std::cerr<<fmt::format("UID : {} : {} of {}", sent.uid.val, uids.front().val, uids.back().val)<<std::endl;
                assert(uids.cend()!=std::find(uids.cbegin(), uids.cend(), sent.uid));
                assert(uid2sent[sent.uid].uid == sent.uid);
            }
            timer.here_then_reset("Prepared next pass in a query chain.");
        }
    }

    wordrep::WordUIDindex const& wordUIDs;
    wordrep::WordImportance const& word_importance;
    wordrep::Sentences const& uid2sent;
    WordSimCache& dists_cache;
    ProcessQuerySent processor;
};

///////////////////////////////////////////////////////////////
template<typename T>
QueryEngine<T>::QueryEngine(json_t const &config)
: word_importance{util::io::h5read(util::get_str(config,"word_prob_dump"))},
  phrase_segmenter{word_importance},
  db{config},
  dbinfo{config},
  queries{{config["wordvec_store"], config["voca_name"],
           config["w2vmodel_name"], config["w2v_float_t"]}, {config}}
{}

template<typename T>
json_t QueryEngine<T>::register_documents(json_t const &ask) {
    if (ask.find("sentences") == ask.end()) return json_t{};
    auto uids = queries.append_chunk(data::CoreNLPjson{ask});

    json_t answer{};
    std::vector<SentUID::val_t> uid_vals;
    for(auto uid :uids ) if(queries.uid2sent[uid].chrlen()>5) uid_vals.push_back(uid.val);
    answer["sent_uids"]=uid_vals;
    std::cerr<<fmt::format("# of sents : {}\n", uid_vals.size()) << std::endl;

    dbinfo.tag_on_register_documents(ask, answer);
    return answer;
}

template<typename T>
json_t QueryEngine<T>::ask_query(json_t const &ask) const {
    if (!dbinfo_t::query_t::is_valid(ask)) return json_t{};
    typename dbinfo_t::query_t query{ask};
    auto max_clip_len = util::find<int64_t>(ask, "max_clip_len").value_or(200);
    auto n_cut = util::find<int64_t>(ask, "n_cut").value_or(5);

    auto query_sents = dbinfo.get_query_sents(query, queries.uid2sent, db.uid2sent);
    auto candidate_sents = dbinfo.get_candidate_sents(query, db);

    ProcessQuerySents query_processor{db.token2uid.word, word_importance, dists_cache};
    util::ConcurrentVector<data::QueryResult> answers;
    auto op_cut =[this,n_cut](auto const& xs){return dbinfo.rank_cut(xs,n_cut);};
    auto op_results = [this,max_clip_len](auto const& query_sent, auto const& scored_sent){
        return dbinfo.build_result(query_sent, scored_sent, max_clip_len);
    };
    auto per_sent=[&answers,max_clip_len,op_cut,op_results](
            auto const &query_sent, auto const& query_sent_info, auto const &relevant_sents){
        data::QueryResult answer;
        answer.results = write_output(query_sent, relevant_sents, op_cut, op_results);
        answer.query = query_sent_info;
        answer.n_relevant_matches = relevant_sents.size();
        answers.push_back(answer);
    };
    query_processor(query_sents, candidate_sents, per_sent);
    return to_json(answers.to_vector());
}

template<typename T>
json_t QueryEngine<T>::ask_chain_query(json_t const &ask) const {
    if (!dbinfo_t::query_t::is_valid(ask)) return json_t{};
    typename  dbinfo_t::query_t query{ask};
    auto max_clip_len = util::find<int64_t>(ask, "max_clip_len").value_or(200);
    auto n_cut = util::find<int64_t>(ask, "n_cut").value_or(5);

    auto query_sents = dbinfo.get_query_sents(query, queries.uid2sent, db.uid2sent);
    auto candidate_sents = dbinfo.get_candidate_sents(query, db);
    auto maybe_sents = util::find<std::vector<int64_t>>(ask, "sents");
    if(maybe_sents) {
        auto uids = util::deserialize<SentUID>(maybe_sents.value());
        std::vector<Sentence> custom_sents;
        for(auto uid : uids) custom_sents.push_back(db.uid2sent.find(uid).value());
        candidate_sents = custom_sents;
    }

    output_t answers{};
    auto op_cut =[this,n_cut](auto const& xs){return dbinfo.rank_cut(xs,n_cut);};
    auto op_results = [this,max_clip_len](auto const& query_sent, auto const& scored_sent){
        return dbinfo.build_result(query_sent, scored_sent, max_clip_len);
    };
    auto per_sent = [this,&answers,max_clip_len,op_cut,op_results](
            auto const &query_sent, auto const &query_sent_info, auto const &relevant_sents){
        for(auto pair : util::zip(query_sent_info.words, query_sent_info.cutoffs)) {
            fmt::print(std::cerr, "{} : {}\n", pair.first, pair.second);
        }
        std::cerr<<std::endl;

        data::QueryResult answer;
        answer.results = write_output(query_sent, relevant_sents, op_cut, op_results);
        answer.query = query_sent_info;
        answer.n_relevant_matches = relevant_sents.size();
        answers.push_back(answer);
        Timer timer;
        int i=0;
        for(auto ssent : relevant_sents){
            if(dbinfo.indexer.column_uid(ssent.sent.tokens->chunk_idx(ssent.sent.beg))!=ColumnUID{3}) continue;
            if(++i>20) break;
            for(auto idx=ssent.sent.beg; idx!=ssent.sent.end; ++idx){
                fmt::print(std::cerr, "{} ", db.token2uid.word[db.tokens.word_uid(idx)]);
            }
            fmt::print(std::cerr, "\n:Original sentence. Phrases:\n");

            auto phrases = phrase_segmenter.broke_into_phrases(ssent.sent, 5.0);
            for (auto phrase : phrases) {
                for (auto idx : phrase.idxs) {
                    fmt::print(std::cerr, "{} ", db.token2uid.word[db.tokens.word_uid(idx)]);
                }
                fmt::print(std::cerr, "\n");
            }
            fmt::print(std::cerr, "-------------------\n");
        }
        timer.here_then_reset("Extract phrases.");
    };

    ProcessChainQuery processor{db.token2uid.word, word_importance, db.uid2sent, dists_cache};
    processor(query_sents, candidate_sents, per_sent);

    return to_json(answers);
}

template<typename T>
json_t QueryEngine<T>::ask_query_stats(json_t const &ask) const {
    std::cerr<<fmt::format("{}\n", ask.dump(4))<<std::endl;
    if (!dbinfo_t::query_t::is_valid(ask)) return json_t{};
    typename dbinfo_t::query_t query{ask};
    auto max_clip_len = util::find<int64_t>(ask, "max_clip_len").value_or(200);
    auto n_cut = util::find<int64_t>(ask, "n_cut").value_or(5);

    auto query_sents = dbinfo.get_query_sents(query, queries.uid2sent, db.uid2sent);
    auto candidate_sents = dbinfo.get_candidate_sents(query, db);

    std::map<SentUID, std::map<WordUID,std::map<WordUID,std::vector<SentUID>>>> results_by_match;
    std::map<SentUID, std::map<WordUID,std::map<WordUID,std::size_t>>> stats;
    auto collect_result_stats = [&results_by_match,&stats](auto const &query_sent, auto const &, auto const &relevant_sents){
        auto sent_uid = query_sent.uid;
        for(auto const &scored_sent : relevant_sents){
            for(auto elm : scored_sent.scores.serialize()){
                auto qidx = std::get<0>(elm);
                auto midx = std::get<1>(elm);
                auto quid = query_sent.tokens->word_uid(qidx);
                auto muid = scored_sent.sent.tokens->word_uid(midx);
                auto score = std::get<2>(elm);
                if(score<0.6) continue;
                ++stats[sent_uid][quid][muid];
                results_by_match[sent_uid][quid][muid].push_back(scored_sent.sent.uid);
            }
        }
    };
    output_t answers{};
    auto op_cut =[this,n_cut](auto const& xs){return dbinfo.rank_cut(xs,n_cut);};
    auto op_results = [this,max_clip_len](auto const& query_sent, auto const& scored_sent){
        return dbinfo.build_result(query_sent, scored_sent, max_clip_len);
    };
    auto collect_query_result = [&answers,max_clip_len,op_cut,op_results](
            auto const &query_sent, auto const &query_sent_info, auto const &relevant_sents){
        for(auto pair : util::zip(query_sent_info.words, query_sent_info.cutoffs)) {
            fmt::print(std::cerr, "{} : {}\n", pair.first, pair.second);
        }
        std::cerr<<std::endl;

        data::QueryResult answer;
        answer.results = write_output(query_sent, relevant_sents, op_cut, op_results);
        answer.query = query_sent_info;
        answer.n_relevant_matches = relevant_sents.size();
        answers.push_back(answer);
    };
    auto op_per_sent=[collect_result_stats,collect_query_result](
            auto const &query_sent, auto const &query_sent_info, auto const &relevant_sents){
        collect_result_stats(query_sent,query_sent_info, relevant_sents);
        collect_query_result(query_sent,query_sent_info, relevant_sents);
    };

    ProcessChainQuery processor{db.token2uid.word, word_importance, db.uid2sent, dists_cache};
    processor(query_sents, candidate_sents, op_per_sent);

    util::json_t stats_output;
    util::json_t stats_output_idxs;
    fmt::print(std::cerr, "Result stats\n");
    for(auto per_sent_stats : stats) {
        auto sent_uid = per_sent_stats.first;
        fmt::print(std::cerr, "For query sent {}:\n", sent_uid.val);

        util::json_t stats_output_per_sent;
        stats_output_idxs.push_back(sent_uid.val);
        for (auto pair : per_sent_stats.second) {
            util::json_t per_qword{};
            auto quid = pair.first;
            for (auto elm : pair.second) {
                auto muid = elm.first;
                for (auto uid : results_by_match[sent_uid][quid][muid]) per_qword[db.token2uid.word[muid]].push_back(uid.val);
                fmt::print(std::cerr, "{:<15} {:<15} : {:<15}\n",
                           db.token2uid.word[quid], db.token2uid.word[muid], elm.second);
            }
            stats_output_per_sent[db.token2uid.word[quid]] = per_qword;
            fmt::print(std::cerr, "------------------\n");
        }
        fmt::print(std::cerr, "==================\n");
        stats_output.push_back(stats_output_per_sent);
    }
    fmt::print(std::cerr, "//////////////////////////////////////////////////\n");

    util::json_t output{};
    util::json_t results = to_json(answers);
    for(auto& result : results) output["results"].push_back(result);
    output["stats"]=stats_output;
    output["stats_uid"] = stats_output_idxs;
    return output;
}

template<typename T>
json_t QueryEngine<T>::ask_sents_content(json_t const &ask) const{
    json_t output{};
    for(int64_t uid : ask["sents"]) {
        //TODO: do not assume uid is from db. It may come from queries.
        auto sent = db.uid2sent[SentUID{uid}];
        auto chunk_idx = db.tokens.chunk_idx(sent.beg);
        auto col_uid = dbinfo.indexer.column_uid(chunk_idx);
        auto row_idx = dbinfo.indexer.row_idx(chunk_idx);

        data::rss::HashIndexer hash2idx{"/home/jihuni/word2vec/nyt/nyt.raw"};
        auto hash = hash2idx.hash(data::rss::HashIndex{row_idx.val});
        auto column = dbinfo.db.column(col_uid);

        auto offset_beg = sent.beg_offset().val;
        auto offset_end = sent.end_offset().val;

        auto row_str = util::string::read_whole(fmt::format("/home/jihuni/word2vec/parsed/{}.{}", hash, column));
        auto substr = util::string::substring_unicode_offset(row_str, offset_beg, offset_end);
        output["sents"].push_back(substr);
    };
    return output;

}


//Explicit instantiation of query engines.
template class engine::QueryEngine<data::rss::DBInfo>;
template class engine::QueryEngine<data::ygp::DBInfo>;

}//namespace engine
