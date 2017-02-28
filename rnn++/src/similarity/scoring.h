#pragma once

#include <vector>

#include "wordrep/dep_parsed.h"
#include "wordrep/voca_info.h"
#include "wordrep/words.h"
#include "wordrep/simiarity_score.h"

#include "data_source/db_query.h"

namespace data{
struct DBIndexer;
}//namespace data

namespace engine{

struct DepSearchScore{
    using val_t = wordrep::VocaInfo::val_t;
    using Index = wordrep::ConsecutiveTokens;
    DepSearchScore(size_t len) : idxs_lhs(len,{-1}),idxs_rhs(len,{-1}), scores(len) {}
    void set(size_t j, Index idx_lhs, Index idx_rhs, val_t score){
        scores[j]=score;
        idxs_lhs[j]=idx_lhs;
        idxs_rhs[j]=idx_rhs;
    }
    std::vector<std::pair<Index,val_t>> scores_with_idx() const {
        return util::zip(idxs_rhs, scores);
    };
    auto insert(Index lhs, wordrep::Scoring::Score rhs) {
        idxs_lhs.push_back(lhs);
        idxs_rhs.push_back(rhs.data);
        scores.push_back(rhs.score);
    }
    auto serialize() const {
        return util::zip(idxs_lhs, idxs_rhs, scores);
    };
    val_t score_sum() const;
private:
    std::vector<Index> idxs_lhs;
    std::vector<Index> idxs_rhs;
    std::vector<val_t> scores;
};

struct ScoredSentence{
    using val_t = DepSearchScore::val_t;
    ScoredSentence(wordrep::Sentence sent, DepSearchScore const &scores)
            :sent{std::move(sent)}, scores{scores}, score{scores.score_sum()} {
    }
    wordrep::Sentence sent;
    DepSearchScore scores;
    val_t score;
};

ScoredSentence output(wordrep::Scoring::ScoredSentence const& sent);

std::vector<ScoredSentence> plain_rank_cut(std::vector<ScoredSentence> relevant_sents,
                                           size_t n_max_result);
std::vector<ScoredSentence> rank_cut_by_unique_chunk(std::vector<ScoredSentence> relevant_sents,
                                                     size_t n_unique_chunk_idx);

data::PerSentQueryResult build_query_result_POD(
        wordrep::Sentence const &query_sent, ScoredSentence const &matched_sentence,
        data::DBIndexer const &db_indexer, int64_t max_clip_len);

}//namespace engine

