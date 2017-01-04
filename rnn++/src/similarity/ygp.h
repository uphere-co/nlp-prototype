#pragma once

#include "data_source/ygp_db.h"

#include "similarity/scoring.h"

#include "utils/json.h"

namespace engine{
struct Dataset;
}

namespace data{
namespace ygp{

std::vector<engine::ScoredSentence> rank_cut_per_column(
        std::vector<engine::ScoredSentence> const &relevant_sents,
        size_t n_max_per_table,
        DBIndexer const &ygp_indexer,
        YGPdb const &ygpdb);
std::vector<engine::ScoredSentence> rank_cut_per_row_index(
        std::vector<engine::ScoredSentence> const &relevant_sents,
        size_t n_max_per_table,
        DBIndexer const &ygp_indexer,
        ygp::YGPdb const &ygpdb);

struct Query{
    using json_t = util::json_t;
    Query(json_t const &ask){
        for(wordrep::SentUID::val_t uid : ask["sent_uids"] ) uids.push_back(wordrep::SentUID{uid});
        for(auto country : ask["Countries"]) countries.push_back(country);
    }
    static bool is_valid(json_t const &query){
        return query.find("sent_uids")!=query.end() && query.find("max_clip_len")!=query.end()
               && query.find("Countries")!=query.end();
    }
    std::vector<wordrep::SentUID> uids;
    std::vector<std::string> countries;
};

struct DBInfo{
    using query_t = Query;
    DBInfo(util::json_t const& config);

    auto rank_cut(std::vector<engine::ScoredSentence> const &relevant_sents, int64_t n_cut) const {
//        return rank_cut_per_column(relevant_sents, n_cut, indexer, db);
        return rank_cut_per_row_index(relevant_sents, n_cut, indexer, db);
    }

    void tag_on_register_documents(util::json_t const& ask, util::json_t& answer) const{
        auto found_countries = country_tagger.tag(ask["query_str"]);
        answer["Countries"]=found_countries;
    }
    std::vector<wordrep::Sentence> get_query_sents(
            query_t const& query,
            wordrep::Sentences const &query_sent_uids,
            wordrep::Sentences const &db_sent_uids) const;
    std::vector<wordrep::Sentence> get_candidate_sents(
            query_t const& query,
            engine::Dataset const& db) const;

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
