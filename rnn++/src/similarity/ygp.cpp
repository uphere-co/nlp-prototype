#include "similarity/ygp.h"

#include "utils/versioned_name.h"
#include "utils/hdf5.h"

using util::io::h5read;
using engine::ScoredSentence;
using engine::plain_rank_cut;

namespace data{
namespace ygp{

DBInfo::DBInfo(util::json_t const& config)
        : db{config["column_uids_dump"].get<std::string>()},
          indexer{h5read(util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname),
                  config["dep_parsed_prefix"].get<std::string>()},
          per_country{h5read(util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname),
                      util::get_latest_version(util::get_str(config, "country_uids_dump")).fullname},
          country_tagger{util::get_latest_version(util::get_str(config, "country_uids_dump")).fullname}
{}

std::vector<ScoredSentence> per_table_rank_cut(
        std::vector<ScoredSentence> const &relevant_sents, size_t n_max_per_table,
        DBIndexer const &ygp_indexer, ygp::YGPdb const &ygpdb){
    std::map<std::string, std::vector<ScoredSentence>> outputs_per_column;
    for(auto const &scored_sent : relevant_sents){
        auto const &sent = scored_sent.sent;
        auto col_uid=ygp_indexer.column_uid(sent.tokens->chunk_idx(sent.beg));
        auto table_name = ygpdb.table(col_uid);
        outputs_per_column[table_name].push_back(scored_sent);
    }
    std::vector<ScoredSentence> top_N_results;
    for(auto const &pair : outputs_per_column){
        util::append(top_N_results, plain_rank_cut(pair.second, n_max_per_table));
    }
    return plain_rank_cut(top_N_results, n_max_per_table*2);
}

}//data::ygp
}//data
