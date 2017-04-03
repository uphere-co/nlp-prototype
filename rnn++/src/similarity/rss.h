#pragma once

#include "data_source/rss.h"

#include "similarity/scoring.h"
#include "similarity/config.h"

#include "utils/json.h"

namespace engine{
struct Dataset;
}

namespace data{
namespace rss{

template<typename T>
struct ConfigKeys{
    std::vector<T> keys={};
};

struct Config{
    Config(util::json_t const& config)
            : common{config}, rss{config}
    {}
    engine::Config common;
    util::ConfigT<ConfigKeys> rss;
};
struct Factory{
    Factory(Config const& config, std::optional<int> data_minor_version={})
            : config{config}, common{config.common, data_minor_version} {}
    Columns db() const;

    Config config;
    engine::SubmoduleFactory common;
};

struct Query{
    using json_t = util::json_t;
    Query(json_t const &ask){
        for(wordrep::SentUID::val_t uid : ask["sent_uids"] ) uids.push_back(wordrep::SentUID{uid});
    }
    static bool is_valid(json_t const &query){
        return query.find("sent_uids")!=query.end() && query.find("max_clip_len")!=query.end();
    }
    std::vector<wordrep::SentUID> uids;
};

struct DBInfo{
    using factory_t = Factory;
    using query_t = Query;
    static void annotation_on_result(util::json_t const& config, util::json_t &answers){
        data::rss::annotation_on_result(config, answers);
    }

    DBInfo(factory_t const& factory);

    auto rank_cut(std::vector<engine::ScoredSentence> const &relevant_sents, int64_t n_cut) const {
        return engine::rank_cut_by_unique_chunk(relevant_sents, n_cut);
    }

    void tag_on_register_documents(util::json_t const& /*ask*/, util::json_t& /*answer*/) const {}
    std::vector<wordrep::Sentence> get_query_sents(
            query_t const& query,
            wordrep::Sentences const &query_sent_uids,
            wordrep::Sentences const &db_sent_uids) const;
    std::vector<wordrep::Sentence> get_candidate_sents(
            query_t const& query,
            engine::Dataset const& db) const;

    PerSentQueryResult build_result(wordrep::Sentence const &query_sent,
                                    engine::ScoredSentence const &matched_sentence,
                                    int64_t max_clip_len) const {
        auto result = build_query_result_POD(query_sent, matched_sentence, indexer, max_clip_len);

        auto sent = matched_sentence.sent;
        auto chunk_idx = sent.dict->chunk_idx(sent.front());
        auto col_uid = indexer.column_uid(chunk_idx);
        result.table_name = db.table(col_uid);
        result.column_name= db.column(col_uid);
        result.index_col_name = db.index_col(col_uid);

        return result;
    }

    Columns const db;
    DBIndexer const indexer;
};

}//data::rss
}//data
