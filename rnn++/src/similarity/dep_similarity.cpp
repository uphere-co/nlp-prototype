#include <algorithm>
#include <map>

#include <fmt/printf.h>

#include "data_source/db_query.h"
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

using data::PerSentQueryResult;
using data::ScoreWithOffset;

namespace {

using engine::ScoredSentence;

auto get_clip_offset = [](Sentence sent, engine::DepSearchScore const &score, auto const &tokens,
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

PerSentQueryResult build_query_result_POD(
        Sentence const &query_sent, ScoredSentence const &matched_sentence,
        ygp::YGPindexer const &db_indexer, int64_t max_clip_len){
    auto const &scores = matched_sentence.scores;
    auto sent = matched_sentence.sent;
    auto scores_with_idxs = scores.serialize();
    std::sort(scores_with_idxs.begin(), scores_with_idxs.end(),
              [](auto const &x, auto const &y){return std::get<2>(x)>std::get<2>(y);});

    auto const &tokens = *(sent.tokens);
    auto const &query_tokens = *(query_sent.tokens);

    auto chunk_idx = tokens.chunk_idx(sent.beg);
    auto row_uid = db_indexer.row_uid(chunk_idx);//if a chunk is a row, chunk_idx is row_uid
    auto col_uid = db_indexer.column_uid(chunk_idx);
    auto row_idx = db_indexer.row_idx(chunk_idx);

    PerSentQueryResult result;
    for(auto elm : scores_with_idxs){
        auto lhs_idx = std::get<0>(elm);
        auto rhs_idx = std::get<1>(elm);
        auto score   = std::get<2>(elm);
        if(score==0.0) continue;
        ScoreWithOffset tmp;
        tmp.score = score;
        tmp.query_word.beg = query_tokens.word_beg(lhs_idx).val;
        tmp.query_word.end = query_tokens.word_end(lhs_idx).val;
        tmp.matched_word.beg = tokens.word_beg(rhs_idx).val;
        tmp.matched_word.end = tokens.word_end(rhs_idx).val;
        result.scores_with_offset.push_back(tmp);
    }
    result.score = scores.score_sum();
    data::set_db_info(result, col_uid, row_uid, row_idx, sent);
    result.highlight_offset = {0,0};
    auto clip_offset = get_clip_offset(sent, scores, tokens, max_clip_len);
    result.clip_offset = {clip_offset.first.val, clip_offset.second.val};

    return result;
}

PerSentQueryResult build_ygp_query_result_POD(Sentence const &query_sent,
                                ScoredSentence const &matched_sentence,
                                int64_t max_clip_len,
                                ygp::YGPdb const &ygpdb,
                                ygp::YGPindexer const &ygp_indexer,
                                ygp::DBbyCountry const &ygpdb_country){
    auto result = build_query_result_POD(query_sent, matched_sentence, ygp_indexer, max_clip_len);

    auto sent = matched_sentence.sent;
    auto chunk_idx = sent.tokens->chunk_idx(sent.beg);
    auto col_uid = ygp_indexer.column_uid(chunk_idx);
    result.table_name = ygpdb.table(col_uid);
    result.column_name= ygpdb.column(col_uid);
    result.index_col_name = ygpdb.index_col(col_uid);

    result.country = ygpdb_country.get_country(sent.uid);

    return result;
}


struct Query{
    using json_t = util::json_t;
    Query(json_t const &ask){
        for(SentUID::val_t uid : ask["sent_uids"] ) uids.push_back(SentUID{uid});
    }
    static bool is_valid(json_t const &query){
        return query.find("sent_uids")!=query.end() && query.find("max_clip_len")!=query.end();
    }
    std::vector<SentUID> uids;
};
struct YGPQuery{
    using json_t = util::json_t;
    YGPQuery(json_t const &ask){
        for(SentUID::val_t uid : ask["sent_uids"] ) uids.push_back(SentUID{uid});
        for(auto country : ask["Countries"]) countries.push_back(country);
    }
    static bool is_valid(json_t const &query){
        return query.find("sent_uids")!=query.end() && query.find("max_clip_len")!=query.end()
               && query.find("Countries")!=query.end();
    }
    std::vector<SentUID> uids;
    std::vector<std::string> countries;
};


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


DepSimilaritySearch::DepSimilaritySearch(json_t const &config)
: voca{config["wordvec_store"], config["voca_name"],
       config["w2vmodel_name"], config["w2v_float_t"]},
  tokens{util::get_latest_version(util::get_str(config, "dep_parsed_store")), config["dep_parsed_prefix"]},
  wordUIDs{config["word_uids_dump"].get<std::string>()},
  posUIDs{config["pos_uids_dump"].get<std::string>()},
  arclabelUIDs{config["arclabel_uids_dump"].get<std::string>()},
  word_importance{H5file{H5name{config["word_prob_dump"].get<std::string>()}, hdf5::FileMode::read_exist}},
  sents{tokens.IndexSentences()},
  uid2sent{sents},
  ygpdb{config["column_uids_dump"].get<std::string>()},
  ygp_indexer{h5read(util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname),
              config["dep_parsed_prefix"].get<std::string>()},
  ygpdb_country{h5read(util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname),
                util::get_latest_version(util::get_str(config, "country_uids_dump")).fullname},
  country_tagger{util::get_latest_version(util::get_str(config, "country_uids_dump")).fullname}
{}

//TODO: fix it to be thread-safe
DepSimilaritySearch::json_t DepSimilaritySearch::register_documents(json_t const &ask) {
    if (ask.find("sentences") == ask.end()) return json_t{};
    std::lock_guard<std::mutex> append_query_toekns{query_tokens_update};
    query_tokens.append_corenlp_output(wordUIDs, posUIDs, arclabelUIDs, data::CoreNLPjson{ask});
    query_tokens.build_voca_index(voca.indexmap);
    auto uids = query_tokens.build_sent_uid(SentUID{SentUID::val_t{0x80000000}});
    queries_sents = query_tokens.IndexSentences();
    uid2query_sent.add(queries_sents);

    json_t answer{};
    std::vector<SentUID::val_t> uid_vals;
    for(auto uid :uids ) if(uid2query_sent[uid].chrlen()>5) uid_vals.push_back(uid.val);
    answer["sent_uids"]=uid_vals;
    std::cerr<<fmt::format("# of sents : {}\n", uid_vals.size()) << std::endl;
    auto found_countries = country_tagger.tag(ask["query_str"]);
    answer["Countries"]=found_countries;
    return answer;
}

DepSimilaritySearch::json_t DepSimilaritySearch::ask_query(json_t const &ask) const {
    if (!YGPQuery::is_valid(ask)) return json_t{};
    YGPQuery query{ask};
    std::vector<Sentence> query_sents{};
    for(auto uid : query.uids){
        auto sent = uid2query_sent.find(uid);
        if(!sent) sent = uid2sent.find(uid);
        if(!sent) continue;
        query_sents.push_back(sent.value());
    }
    fmt::print("Will process {} user documents\n", query_sents.size());

    std::cerr<<"Find for a query in DB of : ";
    for(auto const &country : query.countries) std::cerr<<country << ", ";
    std::cerr<<std::endl;
    if(query.countries.size()==0) std::cerr<<"No countries are specified. Find for all countries."<<std::endl;

    auto uids = ygpdb_country.sents(query.countries);
    std::vector<Sentence> candidate_sents;
    for(auto uid : uids) candidate_sents.push_back(uid2sent[uid]);
    if(query.countries.size()==0) candidate_sents=sents;

    output_t answers = process_query_sents(query_sents, candidate_sents);
    return to_json(answers);
//    auto max_clip_len = ask["max_clip_len"].get<int64_t>();
}

DepSimilaritySearch::json_t DepSimilaritySearch::ask_chain_query(json_t const &ask) const {
    std::cerr<<fmt::format("{}\n", ask.dump(4))<<std::endl;
    if (!Query::is_valid(ask)) return json_t{};
    YGPQuery query{ask};
    std::vector<Sentence> query_sents{};
    for(auto uid : query.uids){
        auto sent = uid2query_sent.find(uid);
        if(!sent) sent = uid2sent.find(uid);
        if(!sent) continue;
        query_sents.push_back(sent.value());
    }
    std::cerr<<"Find for a query in DB of : ";
    for(auto const &country : query.countries) std::cerr<<country << ", ";
    std::cerr<<std::endl;
    if(query.countries.size()==0) std::cerr<<"No countries are specified. Find for all countries."<<std::endl;

    auto uids = ygpdb_country.sents(query.countries);
    std::vector<Sentence> candidate_sents;
    for(auto uid : uids) candidate_sents.push_back(uid2sent[uid]);
    if(query.countries.size()==0) candidate_sents=sents;

    output_t answers = process_chain_query(query_sents, candidate_sents);
    return to_json(answers);
//    auto max_clip_len = ask["max_clip_len"].get<int64_t>();
}

DepSimilaritySearch::output_t DepSimilaritySearch::process_query_sents(
        std::vector<wordrep::Sentence> const &query_sents,
        std::vector<wordrep::Sentence> const &candidate_sents) const {
    auto max_clip_len = 200;
    util::Timer timer{};

    tbb::concurrent_vector<data::QueryResult> answers;
    tbb::task_group g;
    for(auto const &query_sent : query_sents){
        if(query_sent.beg==query_sent.end) continue;
        g.run([&timer,&answers,max_clip_len, query_sent,&candidate_sents, this](){
            if(result_cache.find(query_sent.uid)){
                auto answer = result_cache.get(query_sent.uid);
                answers.push_back(answer);
                timer.here("Query answered using cache.");
                return;
            }
            data::QuerySentInfo info = construct_query_info(query_sent, wordUIDs, word_importance);
            timer.here_then_reset("Get cutoffs");
            cache_words(query_sent, dists_cache);
            timer.here_then_reset("Built Similarity caches.");
            std::cerr<<fmt::format("Query : Find with {} candidate sentences.",candidate_sents.size())<<std::endl;
            auto relevant_sents = this->process_query_sent(query_sent, info.cutoffs, candidate_sents);

            data::QueryResult answer;
            answer.results = write_output(query_sent, relevant_sents, max_clip_len);
            answer.query = info;
            answers.push_back(answer);
            timer.here("Query answered.");
            result_cache.insert(query_sent.uid, answer);
        });
    }
    timer.here_then_reset("All Queries are answered.");
    g.wait();
    output_t output{};
    for(auto &answer : answers) output.push_back(answer);
    return output;
}
DepSimilaritySearch::output_t DepSimilaritySearch::process_chain_query(
        std::vector<wordrep::Sentence> const &query_chain,
        std::vector<wordrep::Sentence> candidate_sents) const {
    auto max_clip_len = 200;
    util::Timer timer{};

    output_t output{};
    for(auto const &query_sent : query_chain){
        if(query_sent.beg==query_sent.end) continue;
        data::QuerySentInfo info = construct_query_info(query_sent, wordUIDs, word_importance);
        cache_words(query_sent, dists_cache);

        std::cerr<<fmt::format("Chain query : Find with {} candidate sentences.",candidate_sents.size())<<std::endl;
        auto relevant_sents = process_query_sent(query_sent, util::deserialize<val_t>(info.cutoffs), candidate_sents);

        auto n0 = candidate_sents.size();
        candidate_sents.clear();
        assert(candidate_sents.size()==0);
        if(!relevant_sents.size()) continue;

        data::QueryResult answer;
        answer.results = write_output(query_sent, relevant_sents, max_clip_len);
        answer.query = info;
        output.push_back(answer);
        timer.here_then_reset("One pass in a query chain is finished.");

        auto best_candidate = std::max_element(relevant_sents.cbegin(), relevant_sents.cend(),
                                               [](auto x, auto y){return x.score<y.score;});
        auto score_cutoff = best_candidate->score * 0.5;
        for(auto scored_sent : relevant_sents){
            if(scored_sent.score < score_cutoff) continue;
            auto sent = scored_sent.sent;
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

std::vector<PerSentQueryResult> DepSimilaritySearch::write_output(
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

    std::vector<PerSentQueryResult> results;
    for(auto const &scored_sent : top_N_results){
        auto result = build_ygp_query_result_POD(query_sent, scored_sent, max_clip_len,
                                             ygpdb, ygp_indexer, ygpdb_country);
        results.push_back(result);
    }

    timer.here_then_reset("Generate JSON output.");
    return results;
}


///////////////////////////////////////////////////////////////
RSSQueryEngine::RSSQueryEngine(json_t const &config)
        : voca{config["wordvec_store"], config["voca_name"],
               config["w2vmodel_name"], config["w2v_float_t"]},
          tokens{util::get_latest_version(util::get_str(config, "dep_parsed_store")), config["dep_parsed_prefix"]},
          wordUIDs{config["word_uids_dump"].get<std::string>()},
          posUIDs{config["pos_uids_dump"].get<std::string>()},
          arclabelUIDs{config["arclabel_uids_dump"].get<std::string>()},
          word_importance{H5file{H5name{config["word_prob_dump"].get<std::string>()}, hdf5::FileMode::read_exist}},
          sents{tokens.IndexSentences()},
          uid2sent{sents},
          rssdb{config["column_uids_dump"].get<std::string>()},
          db_indexer{h5read(util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname),
                      config["dep_parsed_prefix"].get<std::string>()}
{}

RSSQueryEngine::json_t RSSQueryEngine::register_documents(json_t const &ask) {
    if (ask.find("sentences") == ask.end()) return json_t{};
    std::lock_guard<std::mutex> append_query_toekns{query_tokens_update};
    query_tokens.append_corenlp_output(wordUIDs, posUIDs, arclabelUIDs, data::CoreNLPjson{ask});
    query_tokens.build_voca_index(voca.indexmap);
    auto uids = query_tokens.build_sent_uid(SentUID{SentUID::val_t{0x80000000}});
    queries_sents = query_tokens.IndexSentences();
    uid2query_sent.add(queries_sents);

    json_t answer{};
    std::vector<SentUID::val_t> uid_vals;
    for(auto uid :uids ) if(uid2query_sent[uid].chrlen()>5) uid_vals.push_back(uid.val);
    answer["sent_uids"]=uid_vals;
    std::cerr<<fmt::format("# of sents : {}\n", uid_vals.size()) << std::endl;
    return answer;
}

RSSQueryEngine::json_t RSSQueryEngine::ask_query(json_t const &ask) const {
    if (!Query::is_valid(ask)) return json_t{};
    Query query{ask};
    std::vector<Sentence> query_sents{};
    for(auto uid : query.uids){
        auto sent = uid2query_sent.find(uid);
        if(!sent) sent = uid2sent.find(uid);
        if(!sent) continue;
        query_sents.push_back(sent.value());
    }
    fmt::print("Will process {} sentences\n", query_sents.size());
    output_t answers = process_query_sents(query_sents, sents);
    return to_json(answers);
//    auto max_clip_len = ask["max_clip_len"].get<int64_t>();
}

RSSQueryEngine::json_t RSSQueryEngine::ask_chain_query(json_t const &ask) const {
    std::cerr<<fmt::format("{}\n", ask.dump(4))<<std::endl;
    if (!Query::is_valid(ask)) return json_t{};
    auto max_clip_len = ask["max_clip_len"].get<int64_t>();

    Query query{ask};
    std::vector<Sentence> query_sents{};
    for(auto uid : query.uids){
        auto sent = uid2query_sent.find(uid);
        if(!sent) sent = uid2sent.find(uid);
        if(!sent) continue;
        query_sents.push_back(sent.value());
    }
    std::vector<val_t> cutoffs;
    if(ask.find("cutoffs")!=ask.cend()){
        for(auto x : ask["cutoffs"]) std::cerr<<x << std::endl;
        for(auto x : ask["cutoffs"]) cutoffs.push_back(x);
    }
    fmt::print("Will process a query chain of length {}.\n", query_sents.size());

    output_t answers{};
    auto collect_query_result = [this,&answers,max_clip_len](auto const &query_sent, auto const &query_sent_info, auto const &relevant_sents){
        for(auto pair : util::zip(query_sent_info.words, query_sent_info.cutoffs)) {
            fmt::print(std::cerr, "{} : {}\n", pair.first, pair.second);
        }
        std::cerr<<std::endl;

        data::QueryResult answer;
        answer.results = write_output(query_sent, relevant_sents, max_clip_len);
        answer.query = query_sent_info;
        answers.push_back(answer);
    };

    process_chain_query(query_sents, sents, collect_query_result);

    return to_json(answers);
}


RSSQueryEngine::json_t RSSQueryEngine::ask_query_stats(json_t const &ask) const {
    std::cerr<<fmt::format("{}\n", ask.dump(4))<<std::endl;
    if (!Query::is_valid(ask)) return json_t{};
    auto max_clip_len = ask["max_clip_len"].get<int64_t>();

    Query query{ask};
    std::vector<Sentence> query_sents{};
    for(auto uid : query.uids){
        auto sent = uid2query_sent.find(uid);
        if(!sent) sent = uid2sent.find(uid);
        if(!sent) continue;
        query_sents.push_back(sent.value());
    }
    std::vector<val_t> cutoffs;
    if(ask.find("cutoffs")!=ask.cend()){
        for(auto x : ask["cutoffs"]) std::cerr<<x << std::endl;
        for(auto x : ask["cutoffs"]) cutoffs.push_back(x);
    }
    fmt::print("Will process a query chain of length {}.\n", query_sents.size());

    std::map<WordUID,std::map<WordUID,std::vector<SentUID>>> results_by_match;
    std::map<WordUID,std::map<WordUID,std::size_t>> stats;
    auto collect_result_stats = [&results_by_match,&stats](auto const &query_sent, auto const &, auto const &relevant_sents){
        for(auto const &scored_sent : relevant_sents){
            for(auto elm : scored_sent.scores.serialize()){
                auto qidx = std::get<0>(elm);
                auto midx = std::get<1>(elm);
                auto quid = query_sent.tokens->word_uid(qidx);
                auto muid = scored_sent.sent.tokens->word_uid(midx);
                auto score = std::get<2>(elm);
                if(score<0.6) continue;
                ++stats[quid][muid];
                results_by_match[quid][muid].push_back(scored_sent.sent.uid);
            }
        }
    };
    output_t answers{};
    auto collect_query_result = [this,&answers,max_clip_len](auto const &query_sent, auto const &query_sent_info, auto const &relevant_sents){
        for(auto pair : util::zip(query_sent_info.words, query_sent_info.cutoffs)) {
            fmt::print(std::cerr, "{} : {}\n", pair.first, pair.second);
        }
        std::cerr<<std::endl;

        data::QueryResult answer;
        answer.results = write_output(query_sent, relevant_sents, max_clip_len);
        answer.query = query_sent_info;
        answers.push_back(answer);
    };
    auto op_per_sent=[&collect_result_stats,collect_query_result](auto const &query_sent, auto const &query_sent_info, auto const &relevant_sents){
        collect_result_stats(query_sent,query_sent_info, relevant_sents);
        collect_query_result(query_sent,query_sent_info, relevant_sents);
    };

    process_chain_query(query_sents, sents, op_per_sent);

    util::json_t stats_output;
    fmt::print(std::cerr, "Result stats\n");
    for(auto pair : stats){
        util::json_t per_qword{};
        auto quid = pair.first;
        for(auto elm : pair.second){
            auto muid = elm.first;
            for(auto uid : results_by_match[quid][muid]) per_qword[wordUIDs[muid]].push_back(uid.val);
            //per_qword[wordUIDs[muid]]={elm.second, quid.val, muid.val};
            fmt::print(std::cerr, "{:<15} {:<15} : {:<15}\n",
                       wordUIDs[quid], wordUIDs[muid], elm.second);
        }
        stats_output[wordUIDs[quid]]=per_qword;
        fmt::print(std::cerr, "------------------\n");
    }
    fmt::print(std::cerr, "==================\n");

    util::json_t output{};
    util::json_t results = to_json(answers);
    for(auto& result : results) output["results"].push_back(result);
    output["stats"]=stats_output;
    return output;
}

RSSQueryEngine::json_t RSSQueryEngine::ask_sents_content(RSSQueryEngine::json_t const &ask) const{
    json_t output{};
    for(int64_t uid : ask["sents"]) {
        auto sent = uid2sent[SentUID{uid}];
        auto chunk_idx = tokens.chunk_idx(sent.beg);
        auto col_uid = db_indexer.column_uid(chunk_idx);
        auto row_idx = db_indexer.row_idx(chunk_idx);

        data::rss::HashIndexer hash2idx{"/home/jihuni/word2vec/nyt/nyt.raw"};
        auto hash = hash2idx.hash(data::rss::HashIndex{row_idx.val});
        auto column = rssdb.column(col_uid);

        auto offset_beg = sent.beg_offset().val;
        auto offset_end = sent.end_offset().val;

        auto row_str = util::string::read_whole(fmt::format("/home/jihuni/word2vec/parsed/{}.{}", hash, column));
        auto substr = util::string::substring_unicode_offset(row_str, offset_beg, offset_end);
        output["sents"].push_back(substr);
    };
    return output;

}

RSSQueryEngine::output_t RSSQueryEngine::process_query_sents(
        std::vector<wordrep::Sentence> const &query_sents,
        std::vector<Sentence> candidate_sents) const {
    auto max_clip_len = 200;
    tbb::concurrent_vector<data::QueryResult> answers;
    tbb::task_group g;
    util::Timer timer{};

    for(auto const &query_sent : query_sents){
        if(query_sent.beg==query_sent.end) continue;
        g.run([&timer,&answers,max_clip_len, query_sent,&candidate_sents, this](){
            if(result_cache.find(query_sent.uid)){
                auto answer = result_cache.get(query_sent.uid);
                answers.push_back(answer);
                timer.here("Query answered using cache.");
                return;
            }
            data::QuerySentInfo info = construct_query_info(query_sent, wordUIDs, word_importance);
            timer.here_then_reset("Get cutoffs");
            cache_words(query_sent, dists_cache);
            timer.here_then_reset("Built Similarity caches.");
            std::cerr<<fmt::format("Query : Find with {} candidate sentences.",candidate_sents.size())<<std::endl;
            auto relevant_sents = this->process_query_sent(query_sent, info.cutoffs, candidate_sents);

            data::QueryResult answer;
            answer.results = write_output(query_sent, relevant_sents, max_clip_len);
            answer.query = info;
            answers.push_back(answer);
            timer.here("Query answered.");
            result_cache.insert(query_sent.uid, answer);
        });
    }
    timer.here_then_reset("All Queries are answered.");
    g.wait();
    output_t output{};
    for(auto &answer : answers) output.push_back(answer);
    return output;
}

template<typename OP1>
void RSSQueryEngine::process_chain_query(
        std::vector<Sentence> const &query_chain,
        std::vector<Sentence> candidate_sents,
        OP1 const& op_per_sent) const {
    util::Timer timer{};

    for(auto const &query_sent : query_chain){
        if(query_sent.beg==query_sent.end) continue;
        cache_words(query_sent, dists_cache);
        data::QuerySentInfo info = construct_query_info(query_sent, wordUIDs, word_importance, dists_cache);
        timer.here_then_reset("Get cutoffs");

        auto relevant_sents = this->process_query_sent(query_sent, info.cutoffs, candidate_sents);
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

std::vector<ScoredSentence>
RSSQueryEngine::process_query_sent(
        Sentence query_sent, std::vector<val_t> const &cutoffs, std::vector<Sentence> const &data_sents) const {
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


std::vector<data::PerSentQueryResult> RSSQueryEngine::write_output(
        Sentence const &query_sent,
        std::vector<ScoredSentence> const &relevant_sents,
        int64_t max_clip_len) const{
    auto n_found = relevant_sents.size();
    std::cerr<<n_found << " results are found"<<std::endl;

    util::Timer timer;
    auto top_N_results = plain_rank_cut(relevant_sents, 15);
    timer.here_then_reset("Get top N results.");

    std::vector<data::PerSentQueryResult> results;
    for(auto const &scored_sent : top_N_results){
        auto result = build_query_result_POD(query_sent, scored_sent, db_indexer, max_clip_len);
        results.push_back(result);
    }

    timer.here_then_reset("Generate JSON output.");
    return results;
}

}//namespace engine

