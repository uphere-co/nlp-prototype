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
    return {rss.value("column_uids_dump")};
};

DBInfo::DBInfo(Factory const& factory)
        :db{factory.db()},
         indexer{factory.common.db_indexer()}
{}

std::vector<wordrep::Sentence> DBInfo::get_query_sents(
        DBInfo::query_t const& query,
        wordrep::Sentences const &query_sent_uids) const{
    std::vector<Sentence> query_sents{};
    for(auto uid : query.uids){
        auto m_sent = query_sent_uids.find(uid);
        if(m_sent) query_sents.push_back(m_sent.value());
    }
    return query_sents;
}

}//data::ygp
}//data
