#include <algorithm>
#include <map>
#include <utils/profiling.h>

#include "similarity/dep_similarity.h"

#include "similarity/similarity_measure.h"

#include "fmt/printf.h"

#include "utils/parallel.h"
#include "utils/profiling.h"
#include "utils/span.h"
#include "utils/string.h"
#include "utils/hdf5.h"
#include "utils/math.h"

using namespace wordrep;
using namespace util::io;

namespace {
//TODO: merge this with BoWQuery.

template<typename TV>
struct DistanceCache{
    DistanceCache() : val{} {}
    DistanceCache(std::size_t n) : val(n) {}
    DistanceCache(std::vector<TV> const &distances)
            : val{distances} {}
    DistanceCache& operator=(DistanceCache const &obj){
        val = std::move(obj.val);
        return *this;
    }
    TV& operator[](VocaIndex vidx) {return val[vidx.val];}
    TV operator[](VocaIndex vidx) const {return val[vidx.val];}
    std::vector<TV> val;
};
class BoWVQuery2{
public:
    using voca_info_t  = wordrep::VocaInfo;
    using word_block_t = voca_info_t::voca_vecs_t;
    using val_t        = word_block_t::val_t;
    using dist_cache_t = DistanceCache<val_t>;

    BoWVQuery2(std::vector<VocaIndex> const &words,
               voca_info_t const &voca)
    : idxs{words}
    {
        auto n= voca.wvecs.size();
        auto n_queries=idxs.size();
        //for(auto& v: distances) v.resize(n);
        for(auto vidx : idxs) distance_caches[vidx] = dist_cache_t{n};

        tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,10000),
                          [&](tbb::blocked_range<decltype(n)> const &r){
                              for(decltype(n) i=r.begin(); i!=r.end(); ++i){
                                  for(int64_t qi=0; qi!=n_queries; ++qi){
                                      auto q = voca.wvecs[idxs[qi]];
                                      auto widx = VocaIndex{i};
                                      get_distance(WordPosition{qi},widx) = similarity::Similarity<similarity::measure::angle>{}(voca.wvecs[widx], q);
                                  }
                              }
                          });
    }

    const dist_cache_t& distances(VocaIndex widx) const {return distance_caches[widx];}
    const dist_cache_t& distances(WordPosition i) const {return distance_caches[idxs[i.val]];}
    val_t& get_distance(WordPosition i, VocaIndex widx2) { return distance_caches[idxs[i.val]][widx2];}
    val_t get_distance(WordPosition i, VocaIndex widx2) const { return distance_caches[idxs[i.val]][widx2];}
private:
    std::vector<VocaIndex> idxs;
    mutable std::map<VocaIndex,dist_cache_t> distance_caches;
};

//TODO: remove code duplication for parsing CoreNLP outputs
class DepParsedQuery{
public:
    using val_t = BoWVQuery2::val_t;
    DepParsedQuery(std::vector<val_t> const &cutoff, nlohmann::json const &sent,
                   BoWVQuery2 const &similarity)
    : len{cutoff.size()}, cutoff{cutoff}, words_pidx(len), heads_pidx(len), arc_labels(len),
      dists{} {
        for(auto const&x : sent["basicDependencies"]) {
            auto i = x["dependent"].get<int64_t>() - 1;
            words_pidx[i] = WordPosition{x["dependent"].get<WordPosition::val_t>()-1};
            heads_pidx[i] = WordPosition{x["governor"].get<WordPosition::val_t>()-1};
            //TODO: fix it.
            arc_labels[i]= ArcLabelUID{int64_t{0}};//x["dep"];
        }

        for(decltype(len) i=0; i!=len; ++i) sorted_idxs.push_back({cutoff[i],i});
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
        cut = *it * 0.21;
        cut2 = *it * 0.35;
        cut3 = *it * 0.5;

        for(decltype(len)i=0; i!=len; ++i) dists.push_back(&similarity.distances(WordPosition{i}));
    }

    auto get_scores(Sentence const &sent) const {
        auto beg=sent.beg;
        auto end=sent.end;
        val_t total_score{0.0};
        std::vector<std::pair<DPTokenIndex, val_t>>  scores(len);

        auto i_trial{0};
        for(auto pair: sorted_idxs){
            auto j = pair.second;
            val_t score{0.0};
            for(auto i=beg; i!=end; ++i) {
                auto word = sent.tokens->word(i);
                auto head_word = sent.tokens->head_word(i);
                auto dependent_score = (*dists[j])[word];
                if(heads_pidx[j].val<0) {
                    auto tmp = cutoff[j] * dependent_score;
                    if(tmp>score){
                        score = tmp;
                        scores[j] = {i, score};
                    }
                } else {
                    auto governor_score = (*dists[heads_pidx[j].val])[head_word];
                    auto tmp = cutoff[j] * dependent_score * (1 + governor_score)*get_cutoff(heads_pidx[j]);
                    if(tmp>score){
                        score = tmp;
                        scores[j] = {i, score};
                    }
                }
            }
            total_score += score;
            if(++i_trial==n_cut){
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
    val_t get_cutoff (WordPosition idx) const {return cutoff[idx.val];}
    std::size_t n_words() const {return len;}

private:
    std::size_t len;
    std::vector<val_t> cutoff;
    std::vector<WordPosition> words_pidx;
    std::vector<WordPosition> heads_pidx;
    std::vector<ArcLabelUID> arc_labels;
    std::vector<std::pair<val_t,decltype(len)>> sorted_idxs; //Descending order of cutoff.
    std::ptrdiff_t n_cut;
    std::ptrdiff_t n_cut2;
    std::ptrdiff_t n_cut3;
    val_t cut;
    val_t cut2;
    val_t cut3;
    std::vector<BoWVQuery2::dist_cache_t const*> dists;
};


struct SentenceProb{
    int get_rank_cutoff(Sentence const &sent) const {
        return 5;
    }
};

}//nameless namespace

namespace engine {

DepSimilaritySearch::DepSimilaritySearch(json_t const &config)
: voca{config["wordvec_store"], config["voca_name"],
       config["w2vmodel_name"], config["w2v_float_t"]},
  tokens{H5file{H5name{config["dep_parsed_store"].get<std::string>()},
                hdf5::FileMode::read_exist}, config["dep_parsed_prefix"]},
  wordUIDs{config["word_uids_dump"].get<std::string>()},
  posUIDs{config["pos_uids_dump"].get<std::string>()},
  arclabelUIDs{config["arclabel_uids_dump"].get<std::string>()},
  word_cutoff{H5file{H5name{config["word_prob_dump"].get<std::string>()}, hdf5::FileMode::read_exist}},
  sents{tokens.IndexSentences()},
  texts{config["plain_text"].get<std::string>()},
  ygp_indexer{H5file{H5name{config["dep_parsed_store"].get<std::string>()},
                     hdf5::FileMode::read_exist}, config["dep_parsed_prefix"].get<std::string>()}
{}

std::vector<std::string> get_words(nlohmann::json const &sent_json){
    std::vector<std::string> words;
    for(auto const &x : sent_json["basicDependencies"])
        words.push_back(x["dependentGloss"].get<std::string>());
    return words;
}

std::vector<std::tuple<DepSimilaritySearch::val_t, std::vector<std::pair<DPTokenIndex, DepSimilaritySearch::val_t>>, Sentence>>
deduplicate_results(tbb::concurrent_vector<std::tuple<DepSimilaritySearch::val_t, std::vector<std::pair<DPTokenIndex, DepSimilaritySearch::val_t>>, Sentence>> const &relevant_sents){
    using val_t = DepSimilaritySearch::val_t;
    std::map<val_t, bool> is_seen{};
    std::vector<std::tuple<val_t, std::vector<std::pair<DPTokenIndex, val_t>>, Sentence>> dedup_sents;
    for(auto tuple : relevant_sents){
        auto score = std::get<0>(tuple);
        if(is_seen.find(score)!=is_seen.cend()) continue;
        is_seen[score] = true;
        dedup_sents.push_back(tuple);
    }
    return dedup_sents;
}

DepSimilaritySearch::json_t DepSimilaritySearch::process_queries(json_t ask) const {
    json_t output{};
    if (ask.find("sentences") == ask.end() || ask.find("max_clip_len") == ask.end()) return output;
    DepParsedTokens query_tokens{};
    query_tokens.append_corenlp_output(wordUIDs, posUIDs, arclabelUIDs, ask);
    query_tokens.build_sent_uid();
    auto query_sents = query_tokens.IndexSentences();
    auto n_queries = ask["sentences"].size();
    auto max_clip_len = ask["max_clip_len"].get<int64_t>();

    tbb::concurrent_vector<json_t> answers;
    tbb::task_group g;
    assert(query_sents.size()==n_queries);
    for(decltype(n_queries)i=0; i!=n_queries; ++i){
        std::string query_str = ask["queries"][i];
        json_t& sent_json = ask["sentences"][i];
        sent_json["max_clip_len"] = max_clip_len;
        auto query_sent = query_sents[i];
        if(query_sent.beg==query_sent.end) continue;
        auto query_sent_beg = query_tokens.word_beg(query_sent.beg).val;
        auto query_sent_end = query_tokens.word_end(query_sent.end-1).val;
        g.run([&answers,&sent_json,max_clip_len, query_sent_beg,query_sent_end, query_str,this](){
            json_t answer = this->process_query(sent_json);
            answer["input"]=query_str;
            answer["input_offset"]={query_sent_beg,query_sent_end};
            answers.push_back(answer);
        });
    }
    g.wait();
    for(auto &answer : answers) output.push_back(answer);
    return output;
}

DepSimilaritySearch::json_t DepSimilaritySearch::process_query(json_t sent_json) const {
    util::Timer timer{};
    std::vector<std::string> words = get_words(sent_json);
    std::vector<val_t> cutoffs;
    std::vector<VocaIndex> vidxs;
    for(auto const &word : words) {
        auto wuid = wordUIDs[word];
        cutoffs.push_back(word_cutoff.cutoff(wuid));
        auto vuid = voca.indexmap[wuid];
        if(vuid == VocaIndex{}) vuid = voca.indexmap[WordUID{}];
        vidxs.push_back(vuid);
    }
    timer.here_then_reset("Get cutoffs");

    BoWVQuery2 similarity{vidxs, voca};
    timer.here_then_reset("Built Similarity caches.");
    DepParsedQuery query{cutoffs, sent_json, similarity};
    timer.here_then_reset("Query was built.");

    using scores_t = std::vector<std::pair<DPTokenIndex, val_t>>;
    tbb::concurrent_vector<std::tuple<val_t, scores_t, Sentence>> relevant_sents{};
    auto n = sents.size();
    tbb::parallel_for(decltype(n){0}, n, [&](auto i) {
        auto sent = sents[i];
    //for(auto sent: sents) {
        auto scores = query.get_scores(sent);
        val_t score{0.0};
        for(auto pair : scores) score += pair.second;
        if ( score > query.n_words()*0.2) {
            relevant_sents.push_back(std::make_tuple(score,scores, sent));
        }
    });
    auto max_clip_len = sent_json["max_clip_len"].get<int64_t>();
    auto answer = write_output(deduplicate_results(relevant_sents), words, cutoffs, max_clip_len);
    timer.here_then_reset("Query answered.");
    return answer;
}


struct QueryResultBuilder{
    using json_t = DepSimilaritySearch::json_t;
    QueryResultBuilder(){}

    json_t answer;
};

auto get_clip_offset = [](Sentence sent,
                          auto &scores, auto const &tokens, auto max_clip_len){
    std::sort(scores.begin(), scores.end(), [](auto x, auto y){return x.second>y.second;});
    auto pair = scores.front();
    auto i_word_beg = pair.first;
    auto i_word_end = pair.first;
    CharOffset clip_beg = tokens.word_beg(i_word_beg);
    CharOffset clip_end = tokens.word_end(i_word_end);
    auto len_sent = tokens.word_end(sent.end).val - tokens.word_beg(sent.beg).val;
    max_clip_len = max_clip_len>len_sent? len_sent:max_clip_len;
    auto max_len = typename decltype(clip_beg)::val_t{max_clip_len};

    int i_trial=0;
    for(auto pair : scores){
        auto idx = pair.first;
        auto score = pair.second;
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
        if(i_word_beg>sent.beg) {
            --i_word_beg;
            clip_beg = tokens.word_beg(i_word_beg);
        }
        if(i_word_end<sent.end){
            ++i_word_end;
            clip_end = tokens.word_end(i_word_end);
        }
        len = clip_end.val-clip_beg.val;
        if(i_word_beg==sent.beg && i_word_end==sent.end) break;
    }

//    fmt::print("{} {}\n", clip_beg.val, clip_end.val);
    return std::make_pair(clip_beg, clip_end);
};

DepSimilaritySearch::json_t DepSimilaritySearch::write_output(scored_sents_t relevant_sents,
        std::vector<std::string> const &words, std::vector<val_t> const &cutoffs, int64_t max_clip_len) const{
    auto n_found = relevant_sents.size();
    for(size_t i=0; i<words.size(); ++i) {
        fmt::print("{:<10} {:6}.uid {:6}.vocaindex : {}\n", words[i], wordUIDs[words[i]].val,
                   voca.indexmap[wordUIDs[words[i]]].val, cutoffs[i]);
    }
    json_t answer{};
    if(!n_found) return answer;
    auto n_max_result=n_found>5? 5 : n_found;
    auto rank_cut = relevant_sents.begin()+n_max_result;
    std::partial_sort(relevant_sents.begin(),rank_cut,relevant_sents.end(),
                      [](auto const &x, auto const &y){return std::get<0>(x)>std::get<0>(y);});
    auto score_cutoff = 0.5*std::get<0>(relevant_sents[0]);
    rank_cut = std::find_if_not(relevant_sents.begin(), rank_cut,
                                [score_cutoff](auto const &x){return std::get<0>(x)>score_cutoff;});
    for(auto it=relevant_sents.cbegin(); it!=rank_cut; ++it){
        auto const &tuple = *it;
        auto score = std::get<0>(tuple);
        auto scores = std::get<1>(tuple);
        auto sent = std::get<2>(tuple);
        auto chunk_idx = tokens.chunk_idx(sent.beg);
        auto row_uid = ygp_indexer.row_uid(chunk_idx);//if a chunk is a row, chunk_idx is row_uid
        auto col_uid = ygp_indexer.column_uid(chunk_idx);
        auto row_id = ygp_indexer.row_idx(chunk_idx);
        answer["score"].push_back(score);
        auto sent_to_str=[&](auto &sent){
            std::stringstream ss;
            for(auto i=sent.beg; i!=sent.end; ++i) {ss <<  wordUIDs[voca.indexmap[tokens.word(i)]]<< " ";}
            return ss.str();
        };
        answer["result_DEBUG"].push_back(sent_to_str(sent));
        answer["result_row_uid"].push_back(row_uid.val);
        answer["result_row_idx"].push_back(row_id.val);
        answer["result_column_uid"].push_back(col_uid.val);
        auto beg = tokens.word_beg(sent.beg);
        auto end = tokens.word_end(--sent.end);
        auto clip_offset = get_clip_offset(sent, scores, tokens, max_clip_len);
        answer["result_offset"].push_back({beg.val,end.val});
        answer["result_raw"].push_back(texts.getline(row_uid));
        answer["clip_offset"].push_back({clip_offset.first.val, clip_offset.second.val});
        answer["highlight_offset"].push_back({beg.val+10, beg.val+60<end.val?beg.val+60:end.val});
        answer["cutoffs"] = cutoffs; //TODO : meaningless unless user can adjust these
        answer["words"] = words; //TODO: removable?
    }
    return answer;

}

}//namespace engine
