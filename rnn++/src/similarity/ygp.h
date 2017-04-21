#pragma once

#include "similarity/scoring.h"
#include "similarity/config.h"

#include "data_source/ygp_db.h"

#include "utils/json.h"

namespace engine{
struct Dataset;
}

namespace data{
namespace ygp{

template<typename T>
struct ConfigKeys{
    std::vector<T> keys={{"country_uids_dump"}, {"column_uids_dump"}};
};

struct Factory{
    Factory(util::json_t const& config, std::optional<int> data_minor_version={})
            : ygp{config}, common{config, data_minor_version} {}
    YGPdb db() const;
    DBbyCountry db_by_country() const;
    CountryCodeAnnotator country_code_annotator() const;
    std::vector<std::string> country_list() const;

    util::ConfigT<ConfigKeys> ygp;
    engine::SubmoduleFactory common;
};
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
    Query(json_t const &ask)
    : table_columns{} {
        for(wordrep::SentUID::val_t uid : ask["sent_uids"] ) uids.push_back(wordrep::SentUID{uid});
        for(auto country : ask["Countries"]) countries.push_back(country);
        if(ask.find("confine_ygp_table_columns")!=ask.end()){
            std::vector<std::string> tmp;
            for(std::string table_column : ask["confine_ygp_table_columns"]) tmp.push_back(table_column);
            table_columns = tmp;
        }
    }
    static bool is_valid(json_t const &query){
        return query.find("sent_uids")!=query.end() && query.find("max_clip_len")!=query.end()
               && query.find("Countries")!=query.end();
    }
    std::vector<wordrep::SentUID> uids;
    std::vector<std::string> countries;
    std::optional<std::vector<std::string>> table_columns;
};

struct DBInfo{
    using factory_t = Factory;
    using query_t = Query;
    static void annotation_on_result(util::json_t const& config, util::json_t &answers){
        data::ygp::annotation_on_result(config, answers);
    }

    DBInfo(factory_t const& factory);

    auto rank_cut(std::vector<engine::ScoredSentence> const &relevant_sents, int64_t n_cut) const {
        return engine::rank_cut_by_unique_chunk(relevant_sents, n_cut);
        //return rank_cut_per_column(relevant_sents, n_cut, indexer, db);
        //return rank_cut_per_row_index(relevant_sents, n_cut, indexer, db);
    }

    void tag_on_register_documents(util::json_t const& ask, util::json_t& answer) const{
        auto found_countries = country_tagger.tag(ask["query_str"]);
        answer["Countries"]=found_countries;
    }
    std::vector<wordrep::Sentence> get_query_sents(
            query_t const& query,
            wordrep::Sentences const &query_sent_uids) const;

    PerSentQueryResult build_result(wordrep::Sentence const &query_sent,
                                    engine::ScoredSentence const &matched_sentence,
                                    int64_t max_clip_len) const{
        auto result = build_query_result_POD(query_sent, matched_sentence, indexer, max_clip_len);

        auto sent = matched_sentence.sent;
        auto chunk_idx = sent.dict->chunk_idx(sent.front());
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
