#include <algorithm>
#include <map>

#include <fmt/printf.h>

#include "data_source/ygp_query.h"
#include "data_source/ygp_db.h"

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



void QueryResultCache::insert(wordrep::SentUID uid, json_t const&result) {
    data_t::accessor a;
    caches.insert(a, uid);
    a->second = result;
    std::cerr<<fmt::format("Insert {} to a cache", uid.val)<<std::endl;
}
QueryResultCache::json_t QueryResultCache::get(wordrep::SentUID uid) const {
    data_t::const_accessor a;
    if(caches.find(a, uid)) {
        std::cerr<<fmt::format("Cache hits: {}", uid.val)<<std::endl;
        return a->second;
    }
    std::cerr<<fmt::format("Cache misses: {}", uid.val)<<std::endl;
    return json_t{};
}
QueryResultCache::json_t QueryResultCache::find(wordrep::SentUID uid) const {
    data_t::const_accessor a;
    std::cerr<<fmt::format("Look for a cache {} ", uid.val)<<std::endl;
    return caches.find(a, uid);
}


DepSearchScore::val_t DepSearchScore::score_sum() const {return util::math::sum(scores);}



//TODO: remove code duplication for parsing CoreNLP outputs
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
                auto qhead_pidx = query_sent.tokens->head_pos(tidx).val;
                if(qhead_pidx<0) {
                    auto tmp = cutoffs[j] * dependent_score;
                    if(tmp>score){
                        score = tmp;
//                        scores[j] = {i, score};
                        scores.set(j, tidx, i, score);
                    }
                } else {
                    if(cutoffs[qhead_pidx]<0.4) continue;
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

DepSimilaritySearch::DepSimilaritySearch(json_t const &config)
: voca{config["wordvec_store"], config["voca_name"],
       config["w2vmodel_name"], config["w2v_float_t"]},
  tokens{util::get_latest_version(util::get_str(config, "dep_parsed_store")), config["dep_parsed_prefix"]},
  wordUIDs{config["word_uids_dump"].get<std::string>()},
  posUIDs{config["pos_uids_dump"].get<std::string>()},
  arclabelUIDs{config["arclabel_uids_dump"].get<std::string>()},
  word_cutoff{H5file{H5name{config["word_prob_dump"].get<std::string>()}, hdf5::FileMode::read_exist}},
  sents{tokens.IndexSentences()},
  ygpdb{config["column_uids_dump"].get<std::string>()},
  ygp_indexer{read(util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname),
              config["dep_parsed_prefix"].get<std::string>()},
  ygpdb_country{read(util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname),
                config["country_uids_dump"].get<std::string>()},
  country_tagger{config["country_uids_dump"].get<std::string>()}
{}

//TODO: fix it to be thread-safe
DepSimilaritySearch::json_t DepSimilaritySearch::register_documents(json_t const &ask) {
    if (ask.find("sentences") == ask.end()) return json_t{};
    std::lock_guard<std::mutex> append_query_toekns{query_tokens_update};
    query_tokens.append_corenlp_output(wordUIDs, posUIDs, arclabelUIDs, data::CoreNLPjson{ask});
    query_tokens.build_voca_index(voca.indexmap);
    auto uids = query_tokens.build_sent_uid(SentUID{SentUID::val_t{0x80000000}});
    auto sents = query_tokens.IndexSentences();
    Sentences uid2sent{sents};
    json_t answer{};
    std::vector<SentUID::val_t> uid_vals;
    for(auto uid :uids ) if(uid2sent[uid].chrlen()>5) uid_vals.push_back(uid.val);
    answer["sent_uids"]=uid_vals;
    std::cerr<<fmt::format("# of sents : {}\n", uid_vals.size()) << std::endl;
    auto found_countries = country_tagger.tag(ask["query_str"]);
    answer["Countries"]=found_countries;
    return answer;
}

struct Query{
    using json_t = DepSimilaritySearch::json_t;
    Query(json_t const &ask){
        for(SentUID::val_t uid : ask["sent_uids"] ) uids.push_back(SentUID{uid});
    }
    static bool is_valid(json_t const &query){
        return query.find("sent_uids")!=query.end() && query.find("max_clip_len")!=query.end();
    }
    std::vector<SentUID> uids;
};
DepSimilaritySearch::json_t DepSimilaritySearch::ask_query(json_t const &ask) const {
    if (!Query::is_valid(ask)) return json_t{};
    Query query{ask};
    std::vector<Sentence> query_sents{};
    //TODO: fix it to be incremental
    auto qsents = query_tokens.IndexSentences();
    for(auto uid : query.uids){
        auto it = std::find_if(sents.cbegin(), sents.cend(), [uid](auto sent){return sent.uid==uid;});
        if(it==sents.cend()) it=std::find_if(qsents.cbegin(), qsents.cend(), [uid](auto sent){return sent.uid==uid;});
        if(it==qsents.cend()) continue;
        auto sent = *it;
        query_sents.push_back(sent);
    }
    std::vector<std::string> countries;
    for(auto country : ask["Countries"]) countries.push_back(country);
    std::cerr<<"Find for a query in DB of : ";
    for(auto country : ask["Countries"]) std::cerr<<country << ", ";
    std::cerr<<std::endl;
    fmt::print("Will process {} user documents\n", query_sents.size());
    auto results = process_query_sents(query_sents, countries);
    return results;
//    auto max_clip_len = ask["max_clip_len"].get<int64_t>();
}

DepSimilaritySearch::json_t DepSimilaritySearch::ask_chain_query(json_t const &ask) const {
    std::cerr<<fmt::format("{}\n", ask.dump(4))<<std::endl;
    if (!Query::is_valid(ask)) return json_t{};
    Query query{ask};
    std::vector<Sentence> query_sents{};
    //TODO: fix it to be incremental
    auto qsents = query_tokens.IndexSentences();
    for(auto uid : query.uids){
        auto it = std::find_if(sents.cbegin(), sents.cend(), [uid](auto sent){return sent.uid==uid;});
        if(it==sents.cend()) it=std::find_if(qsents.cbegin(), qsents.cend(), [uid](auto sent){return sent.uid==uid;});
        if(it==qsents.cend()) continue;
        auto sent = *it;
        query_sents.push_back(sent);
    }
    std::vector<std::string> countries;
    for(auto country : ask["Countries"]) countries.push_back(country);
    std::cerr<<"Find for a query in DB of : ";
    for(auto country : ask["Countries"]) std::cerr<<country << ", ";
    std::cerr<<std::endl;
    fmt::print("Will process a query chain of length {}.\n", query_sents.size());
    auto results = process_chain_query(query_sents, countries);
    return results;
//    auto max_clip_len = ask["max_clip_len"].get<int64_t>();
}

/*
void matched_highlighter(Sentence sent_ref, Sentence sent,
                         std::vector<DepSimilaritySearch::val_t> const &cutoffs,
                         WordSimCache &dists_cache){
    for(auto ridx=sent_ref.beg; ridx!=sent_ref.end; ++ridx){
        auto ref_vidx = sent_ref.tokens->word(ridx);
        auto ref_head_vidx = sent_ref.tokens->head_word(ridx);
//        auto ref_offset_beg = sent_ref.tokens->word_beg(ridx);
//        auto ref_offset_end = sent_ref.tokens->word_end(ridx);

        auto j = util::diff(ridx, sent_ref.beg);
        auto j_head = util::diff(sent_ref.tokens->head_pos(ridx), sent_ref.tokens->head_pos(sent_ref.beg));
        for(auto widx=sent.beg; widx!=sent.end; ++widx){
            auto word_vidx = sent.tokens->word(widx);
            auto word_head_vidx = sent.tokens->head_word(widx);

            auto dependent_score = dists_cache.distances(ref_vidx)[word_vidx];
            auto governor_score = dists_cache.distances(ref_head_vidx)[word_head_vidx];
            //cutoffs[j] * dependent_score * (1 + governor_score*cutoffs[j_head]);
        }
    }
}
*/

DepSimilaritySearch::json_t DepSimilaritySearch::process_query_sents(
        std::vector<wordrep::Sentence> const &query_sents,
        std::vector<std::string> const &countries) const {
    auto max_clip_len = 200;
    tbb::concurrent_vector<json_t> answers;
    tbb::task_group g;
    util::Timer timer{};

    Sentences uid2sent{sents};
    std::vector<Sentence> candidate_sents;
    std::vector<SentUID> uids;
    for(auto country : countries) append(uids, ygpdb_country.sents(country));

    for(auto uid : uids) candidate_sents.push_back(uid2sent[uid]);
    if(countries.size()==0) {
        std::cerr<<"No countries are specified. Find for all countries."<<std::endl;
        candidate_sents=sents;
    }

    for(auto const &query_sent : query_sents){
        if(query_sent.beg==query_sent.end) continue;
        g.run([&timer,&answers,max_clip_len, query_sent,&candidate_sents, this](){
            if(result_cache.find(query_sent.uid)){
                auto answer = result_cache.get(query_sent.uid);
                answers.push_back(answer);
                timer.here("Query answered using cache.");
                return;
            }
            std::vector<val_t> cutoffs;
            std::vector<VocaIndex> vidxs;
            std::vector<std::string> words;
            for(auto idx = query_sent.beg; idx!=query_sent.end; ++idx) {
                auto wuid = query_sent.tokens->word_uid(idx);
                auto word = wordUIDs[wuid];
                words.push_back(word);
                cutoffs.push_back(word_cutoff.cutoff(wuid));
                auto vuid = voca.indexmap[wuid];
                if(vuid == VocaIndex{}) vuid = voca.indexmap[WordUID{}];
                vidxs.push_back(vuid);
            }
            timer.here_then_reset("Get cutoffs");
            dists_cache.cache(vidxs);
            timer.here_then_reset("Built Similarity caches.");
            std::cerr<<fmt::format("Query : Find with {} candidate sentences.",candidate_sents.size())<<std::endl;
            auto relevant_sents = this->process_query_sent(query_sent, cutoffs, candidate_sents);
            auto answer = write_output(query_sent, relevant_sents, max_clip_len);
            auto query_sent_beg = query_sent.tokens->word_beg(query_sent.beg).val;
            auto query_sent_end = query_sent.tokens->word_end(query_sent.end-1).val;
            answer["input_offset"]={query_sent_beg,query_sent_end};
            answer["input_uid"] = query_sent.uid.val;
            answer["cutoffs"] = cutoffs;
            answer["words"] = words;
            answers.push_back(answer);
            timer.here("Query answered.");
            result_cache.insert(query_sent.uid, answer);
        });
    }
    timer.here_then_reset("All Queries are answered.");
    g.wait();
    json_t output{};
    for(auto &answer : answers) output.push_back(answer);
    return output;
}

DepSimilaritySearch::json_t DepSimilaritySearch::process_chain_query(
        std::vector<wordrep::Sentence> const &query_chain,
        std::vector<std::string> const &countries) const {
    auto max_clip_len = 200;
    util::Timer timer{};
    Sentences uid2sent{sents};
    std::vector<Sentence> candidate_sents;
    std::vector<SentUID> uids;
    for(auto country : countries) {
        append(uids, ygpdb_country.sents(country));
        std::cerr<<fmt::format("Add {}. {} sentences.",country, uids.size())<<std::endl;
    }
    for(auto uid : uids) candidate_sents.push_back(uid2sent[uid]);
    if(countries.size()==0) {
        std::cerr<<"No countries are specified. Find for all countries."<<std::endl;
        candidate_sents=sents;
    }
    auto n0 = sents.size();

    json_t output{};
    for(auto const &query_sent : query_chain){
        if(query_sent.beg==query_sent.end) continue;
        std::vector<val_t> cutoffs;
        std::vector<VocaIndex> vidxs;
        std::vector<std::string> words;
        for(auto idx = query_sent.beg; idx!=query_sent.end; ++idx) {
            auto wuid = query_sent.tokens->word_uid(idx);
            auto word = wordUIDs[wuid];
            words.push_back(word);
            auto cutoff = word_cutoff.cutoff(wuid);
            cutoffs.push_back(cutoff>1.0?0.0:cutoff);
            auto vuid = voca.indexmap[wuid];
            if(vuid == VocaIndex{}) vuid = voca.indexmap[WordUID{}];
            vidxs.push_back(vuid);
        }
        timer.here_then_reset("Get cutoffs");
        dists_cache.cache(vidxs);
        timer.here_then_reset("Built Similarity caches.");
        std::cerr<<fmt::format("Chain query : Find with {} candidate sentences.",candidate_sents.size())<<std::endl;
        auto relevant_sents = this->process_query_sent(query_sent, cutoffs, candidate_sents);
        candidate_sents.clear();
        assert(candidate_sents.size()==0);
        if(!relevant_sents.size()) continue;

        auto answer = write_output(query_sent, relevant_sents, max_clip_len);
        auto query_sent_beg = query_sent.tokens->word_beg(query_sent.beg).val;
        auto query_sent_end = query_sent.tokens->word_end(query_sent.end-1).val;
        answer["input_offset"]={query_sent_beg,query_sent_end};
        answer["input_uid"] = query_sent.uid.val;
        answer["cutoffs"] = cutoffs;
        answer["words"] = words;
        output.push_back(answer);
        timer.here_then_reset("One pass in a query chain is finished.");

        auto best_candidate = std::max_element(relevant_sents.cbegin(), relevant_sents.cend(),
                                               [](auto x, auto y){return x.score<y.score;});
        auto score_cutoff = best_candidate->score * 0.5;
        for(auto scored_sent : relevant_sents){
            if(scored_sent.score < score_cutoff) continue;
            auto sent = scored_sent.sent;
//            candidate_sents.push_back(sent);
            auto uids = sent.tokens->sentences_in_chunk(sent);
            for(auto uid : uids) candidate_sents.push_back(uid2sent[uid]);
            //std::cerr<<fmt::format("UID : {} : {} of {}", sent.uid.val, uids.front().val, uids.back().val)<<std::endl;
            assert(uids.cend()!=std::find(uids.cbegin(), uids.cend(), sent.uid));
            assert(uid2sent[sent.uid].uid == sent.uid);
        }
        std::cerr<<fmt::format("score cutoff {} : {} of {} passed.", score_cutoff, candidate_sents.size(), n0)<<std::endl;
        timer.here_then_reset("Prepared next pass in a query chain.");
    }
    return output;
}


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


std::vector<ScoredSentence>
DepSimilaritySearch::process_query_sent(Sentence query_sent,
                                        std::vector<val_t> const &cutoffs,
                                        std::vector<Sentence> const &data_sents) const {
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

auto get_clip_offset = [](Sentence sent, DepSearchScore const &score, auto const &tokens,
                          auto max_clip_len)->std::pair<CharOffset,CharOffset> {
    auto scores = score.scores_with_idx();
    if(!scores.size()) return {{},{}};
    std::sort(scores.begin(), scores.end(), [](auto x, auto y){return x.second>y.second;});
    auto pair = scores.front();
    auto i_word_beg = pair.first;
    auto i_word_end = pair.first;
    CharOffset clip_beg = tokens.word_beg(i_word_beg);
    CharOffset clip_end = tokens.word_end(i_word_end);
    auto len_sent = sent.chrlen();
    max_clip_len = max_clip_len>len_sent? len_sent:max_clip_len;
    auto max_len = typename decltype(clip_beg)::val_t{max_clip_len};

    for(auto pair : scores){
        auto idx = pair.first;
        //auto score = pair.second;
        auto beg = tokens.word_beg(idx);
        auto end = tokens.word_end(idx);
        if(beg<clip_beg && clip_end < beg+max_len ) {
            clip_beg = beg;
            i_word_beg = idx;
        } else if(end>clip_end && end < clip_beg+max_len ) {
            clip_end = end;
            i_word_end = idx;
        }
//        fmt::print("{} {} {} {}\n", idx.val, score, tokens.word_beg(idx).val, tokens.word_end(idx).val);
    }
    auto len = clip_end.val-clip_beg.val;
    while(max_len-len>0) {
        if(i_word_beg>sent.front()) {
            --i_word_beg;
            clip_beg = tokens.word_beg(i_word_beg);
        }
        if(i_word_end<sent.back()){
            ++i_word_end;
            clip_end = tokens.word_end(i_word_end);
        }
        len = clip_end.val-clip_beg.val;
        if(i_word_beg==sent.front() && i_word_end==sent.back()) break;
    }

//    fmt::print("{} {}\n", clip_beg.val, clip_end.val);
    return {clip_beg, clip_end};
};


std::vector<ScoredSentence> plain_rank_cut(std::vector<ScoredSentence> relevant_sents,
                                           size_t n_max_result){
    auto n_found = relevant_sents.size();
    if(!n_found) return relevant_sents;
    auto n_cut = std::min(n_max_result, n_found);
    auto beg = relevant_sents.begin();
    auto rank_cut = beg+n_cut;
    std::partial_sort(beg,rank_cut,relevant_sents.end(),
                      [](auto const &x, auto const &y){return x.score > y.score;});
    auto score_cutoff = 0.5*relevant_sents.front().score;
    rank_cut = std::find_if_not(beg, rank_cut,
                                [score_cutoff](auto const &x){return x.score>score_cutoff;});
    std::vector<ScoredSentence> top_n_results;
    std::copy(beg, rank_cut, std::back_inserter(top_n_results));
    return top_n_results;
}

std::vector<ScoredSentence> per_table_rank_cut(
        std::vector<ScoredSentence> const &relevant_sents, size_t n_max_per_table,
        ygp::YGPindexer const &ygp_indexer, ygp::YGPdb const &ygpdb){
    std::map<std::string, std::vector<ScoredSentence>> outputs_per_column;
    for(auto const &scored_sent : relevant_sents){
        auto const &sent = scored_sent.sent;
        auto col_uid=ygp_indexer.column_uid(sent.tokens->chunk_idx(sent.beg));
        auto table_name = ygpdb.table(col_uid);
        outputs_per_column[table_name].push_back(scored_sent);
    }
    std::vector<ScoredSentence> top_N_results;
    for(auto const &pair : outputs_per_column){
        util::append(top_N_results, plain_rank_cut(pair.second, n_max_per_table));
    }
    return plain_rank_cut(top_N_results, n_max_per_table*2);
}



ygp::PerSentQueryResult build_query_result_POD(Sentence const &query_sent,
                                          ScoredSentence const &matched_sentence,
                                          int64_t max_clip_len,
                                          ygp::YGPdb const &ygpdb,
                                          ygp::YGPindexer const &ygp_indexer,
                                          ygp::DBbyCountry const &ygpdb_country){
    auto const &scores = matched_sentence.scores;
    auto sent = matched_sentence.sent;
    auto scores_with_idxs = scores.serialize();
    std::sort(scores_with_idxs.begin(), scores_with_idxs.end(),
              [](auto const &x, auto const &y){return std::get<2>(x)>std::get<2>(y);});

    auto const &tokens = *(sent.tokens);
    auto const &query_tokens = *(query_sent.tokens);

    auto chunk_idx = tokens.chunk_idx(sent.beg);
    auto row_uid = ygp_indexer.row_uid(chunk_idx);//if a chunk is a row, chunk_idx is row_uid
    auto col_uid = ygp_indexer.column_uid(chunk_idx);
    auto row_idx = ygp_indexer.row_idx(chunk_idx);

    ygp::PerSentQueryResult result;
    for(auto elm : scores_with_idxs){
        auto lhs_idx = std::get<0>(elm);
        auto rhs_idx = std::get<1>(elm);
        auto score   = std::get<2>(elm);
        if(score==0.0) continue;
        ygp::ScoreWithOffset tmp;
        tmp.score = score;
        tmp.query_word.beg = query_tokens.word_beg(lhs_idx).val;
        tmp.query_word.end = query_tokens.word_end(lhs_idx).val;
        tmp.matched_word.beg = tokens.word_beg(rhs_idx).val;
        tmp.matched_word.end = tokens.word_end(rhs_idx).val;
        result.scores_with_offset.push_back(tmp);
    }
    result.score = scores.score_sum();
    result.result_sent_country = ygpdb_country.get_country(sent.uid);
    result.result_sent_uid = sent.uid.val;
    result.result_row_uid  = row_uid.val;
    result.result_row_idx  = row_idx.val;
    result.result_column_uid  = col_uid.val;
    result.result_table_name = ygpdb.table(col_uid);
    result.result_column_name= ygpdb.column(col_uid);
    result.result_index_col_name = ygpdb.index_col(col_uid);
    result.result_offset  = {sent.beg_offset().val, sent.end_offset().val};
    result.highlight_offset = {0,0};
    auto clip_offset = get_clip_offset(sent, scores, tokens, max_clip_len);
    result.clip_offset = {clip_offset.first.val, clip_offset.second.val};

    return result;
}

util::json_t to_json(std::vector<ygp::PerSentQueryResult> const &results){
    util::json_t answer{};

    for(auto const &result : results){
        answer["score"].push_back(result.score);
        answer["result_sent_country"].push_back(result.result_sent_country);
        answer["result_sent_uid"].push_back(result.result_sent_uid);
        answer["result_row_uid"].push_back(result.result_row_uid);
        answer["result_row_idx"].push_back(result.result_row_idx);
        answer["result_table_name"].push_back(result.result_table_name);
        answer["result_column_name"].push_back(result.result_column_name);
        answer["result_index_col_name"].push_back(result.result_index_col_name);
        answer["result_column_uid"].push_back(result.result_column_uid);
        auto tmp = result.result_offset;
        answer["result_offset"].push_back({tmp.beg, tmp.end});
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

DepSimilaritySearch::json_t DepSimilaritySearch::write_output(
        Sentence const &query_sent,
        std::vector<ScoredSentence> const &relevant_sents,
        int64_t max_clip_len) const{
    auto n_found = relevant_sents.size();
    std::cerr<<n_found << " results are found"<<std::endl;
    json_t answer{};

    util::Timer timer;
//    auto top_N_results = plain_rank_cut(relevant_sents, 5);
    auto top_N_results  = per_table_rank_cut(relevant_sents, 5, ygp_indexer, ygpdb);
    timer.here_then_reset("Get top N results.");

    std::vector<ygp::PerSentQueryResult> results;
    for(auto const &scored_sent : top_N_results){
        auto result = build_query_result_POD(query_sent, scored_sent, max_clip_len,
                                             ygpdb, ygp_indexer, ygpdb_country);
        results.push_back(result);
    }

    timer.here_then_reset("Generate JSON output.");
    return to_json(results);
}

}//namespace engine

