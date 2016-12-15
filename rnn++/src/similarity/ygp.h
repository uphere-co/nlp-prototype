#pragma once

#include "data_source/ygp_db.h"

#include "similarity/scoring.h"

#include "utils/json.h"

namespace data{
namespace ygp{

std::vector<engine::ScoredSentence> per_table_rank_cut(
        std::vector<engine::ScoredSentence> const &relevant_sents, size_t n_max_per_table,
        DBIndexer const &ygp_indexer, YGPdb const &ygpdb);

struct DBInfo{
    DBInfo(util::json_t const& config);

    auto rank_cut(std::vector<engine::ScoredSentence> const &relevant_sents) const {
        return per_table_rank_cut(relevant_sents, 5, indexer, db);
    }


    PerSentQueryResult build_result(wordrep::Sentence const &query_sent,
                                    engine::ScoredSentence const &matched_sentence,
                                    int64_t max_clip_len) const{
        auto result = build_query_result_POD(query_sent, matched_sentence, indexer, max_clip_len);

        auto sent = matched_sentence.sent;
        auto chunk_idx = sent.tokens->chunk_idx(sent.beg);
        auto col_uid = indexer.column_uid(chunk_idx);
        result.table_name = db.table(col_uid);
        result.column_name= db.column(col_uid);
        result.index_col_name = db.index_col(col_uid);

        result.country = per_country.get_country(sent.uid);

        return result;
    }

    YGPdb const db;
    DBIndexer const indexer;
    DBbyCountry const per_country;
    CountryCodeAnnotator const country_tagger;
};


}//data::ygp
}//data
