#pragma once

#include "data_source/rss.h"

#include "similarity/scoring.h"

#include "utils/json.h"

namespace engine{
struct Dataset;
}

namespace data{
namespace rss{

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
    using query_t = Query;

    DBInfo(util::json_t config);

    auto rank_cut(std::vector<engine::ScoredSentence> const &relevant_sents) const {
        return engine::plain_rank_cut(relevant_sents, 15);
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
                                    int64_t max_clip_len) const {
        return build_query_result_POD(query_sent, matched_sentence, indexer, max_clip_len);

    }

    Columns const db;
    DBIndexer const indexer;
};

}//data::rss
}//data
