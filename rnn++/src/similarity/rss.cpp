#include "similarity/rss.h"
#include "similarity/dataset.h"

#include "utils/versioned_name.h"
#include "utils/hdf5.h"

using util::io::h5read;
using wordrep::Sentence;
using engine::ScoredSentence;
using engine::plain_rank_cut;

namespace data{
namespace rss{

Columns Factory::db() const{
    return {config.common.value("column_uids_dump")};
};

HashIndexer Factory::hash_indexer() const{
    return {config.rss.value("row_hashes")};
}

DBInfo::DBInfo(Factory const& factory)
        :db{factory.db()},
         indexer{factory.common.db_indexer()}
{}

std::vector<wordrep::Sentence> DBInfo::get_query_sents(
        DBInfo::query_t const& query,
        wordrep::Sentences const &query_sent_uids,
        wordrep::Sentences const &db_sent_uids) const{
    std::vector<Sentence> query_sents{};
    for(auto uid : query.uids){
        auto sent = query_sent_uids.find(uid);
        if(!sent) sent = db_sent_uids.find(uid);
        if(!sent) continue;
        query_sents.push_back(sent.value());
    }
    return query_sents;
}
std::vector<wordrep::Sentence> DBInfo::get_candidate_sents(
        DBInfo::query_t const& /*query*/, engine::Dataset const& db) const{
    return db.sents;
}

}//data::ygp
}//data
