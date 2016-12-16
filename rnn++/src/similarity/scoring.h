#pragma once

#include <vector>

#include "wordrep/dep_parsed.h"
#include "wordrep/voca_info.h"

#include "data_source/db_query.h"

namespace data{
struct DBIndexer;
}//namespace data

namespace engine{

struct DepSearchScore{
    using val_t = wordrep::VocaInfo::val_t;
    using DPTokenIndex = wordrep::DPTokenIndex;
    DepSearchScore(size_t len) : idxs_lhs(len),idxs_rhs(len), scores(len) {}
    void set(size_t j, DPTokenIndex idx_lhs, DPTokenIndex idx_rhs, val_t score){
        scores[j]=score;
        idxs_lhs[j]=idx_lhs;
        idxs_rhs[j]=idx_rhs;
    }
    std::vector<std::pair<DPTokenIndex,val_t>> scores_with_idx() const {
        return util::zip(idxs_rhs, scores);
    };
    auto serialize() const {
        return util::zip(idxs_lhs, idxs_rhs, scores);
    };
    val_t score_sum() const;
private:
    std::vector<DPTokenIndex> idxs_lhs;
    std::vector<DPTokenIndex> idxs_rhs;
    std::vector<val_t> scores;
};

struct ScoredSentence{
    using val_t = DepSearchScore::val_t;
    ScoredSentence(wordrep::Sentence sent, DepSearchScore const &scores)
            :sent{sent}, scores{scores}, score{scores.score_sum()} {
    }
    wordrep::Sentence sent;
    DepSearchScore scores;
    val_t score;
};


std::vector<ScoredSentence> plain_rank_cut(std::vector<ScoredSentence> relevant_sents,
                                           size_t n_max_result);
data::PerSentQueryResult build_query_result_POD(
        wordrep::Sentence const &query_sent, ScoredSentence const &matched_sentence,
        data::DBIndexer const &db_indexer, int64_t max_clip_len);

}//namespace engine