#include "similarity/ygp.h"
#include "similarity/dataset.h"

#include "utils/versioned_name.h"
#include "utils/hdf5.h"
#include "utils/math.h"
#include "utils/algorithm.h"

using util::io::h5read;
using wordrep::Sentence;
using engine::ScoredSentence;
using engine::plain_rank_cut;

namespace data{
namespace ygp{

std::vector<ScoredSentence> rank_cut_per_column(
        std::vector<ScoredSentence> const &relevant_sents,
        size_t n_max_per_table,
        DBIndexer const &ygp_indexer,
        ygp::YGPdb const &ygpdb){
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
    //return plain_rank_cut(top_N_results, n_max_per_table*2);
    return top_N_results;
}

auto score_sum(std::vector<ScoredSentence> const& sents){
    return util::math::sum(util::map(sents, [](auto const& sent){return sent.score;}));
}
std::vector<ScoredSentence> top_n_per_row_index(
        std::vector<std::vector<ScoredSentence>> relevant_sents,
        size_t n){
    auto n_found = relevant_sents.size();
    if(!n_found) return {};
    auto n_cut = std::min(n, n_found);
    auto beg = relevant_sents.begin();
    auto rank_cut = beg+n_cut;
    std::partial_sort(beg,rank_cut,relevant_sents.end(),
                      [](auto const &x, auto const &y){return score_sum(x) > score_sum(y);});
    auto score_cutoff = 0.5*score_sum(relevant_sents.front());
    rank_cut = std::find_if_not(beg, rank_cut,
                                [score_cutoff](auto const &x){return score_sum(x)>score_cutoff;});
    std::vector<ScoredSentence> top_n_results;
    for(auto it=beg; it!=rank_cut; ++it)
        util::append(top_n_results, *it);
    return top_n_results;
}

std::vector<ScoredSentence> rank_cut_per_row_index(
        std::vector<ScoredSentence> const &relevant_sents,
        size_t n_max_per_table,
        DBIndexer const &ygp_indexer,
        ygp::YGPdb const &ygpdb){
    using ssents_per_rowidx = std::map<RowIndex,std::vector<ScoredSentence>>;
    std::map<std::string, ssents_per_rowidx> outputs_per_row_index;
    for(auto const &scored_sent : relevant_sents){
        auto const &sent = scored_sent.sent;
        auto cidx = sent.tokens->chunk_idx(sent.beg);
        auto table_name = ygpdb.table(ygp_indexer.column_uid(cidx));
        auto row_idx=ygp_indexer.row_idx(cidx);
        outputs_per_row_index[table_name][row_idx].push_back(scored_sent);
    }
    std::vector<ScoredSentence> top_N_results;
    for(auto const &pair : outputs_per_row_index){
        util::append(top_N_results, top_n_per_row_index(util::get_values(pair.second), n_max_per_table));
    }
    //return plain_rank_cut(top_N_results, n_max_per_table*2);
    return top_N_results;
}

DBInfo::DBInfo(util::json_t const& config)
        : db{config["column_uids_dump"].get<std::string>()},
          indexer{h5read(util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname),
                  config["dep_parsed_prefix"].get<std::string>()},
          per_country{h5read(util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname),
                      util::get_latest_version(util::get_str(config, "country_uids_dump")).fullname},
          country_tagger{util::get_latest_version(util::get_str(config, "country_uids_dump")).fullname}
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
        query_t const& query, engine::Dataset const& db) const{
    std::cerr<<"Find for a query in DB of : ";
    for(auto const &country : query.countries) std::cerr<<country << ", ";
    std::cerr<<std::endl;
    if(query.countries.size()==0) std::cerr<<"No countries are specified. Find for all countries."<<std::endl;

    auto uids = per_country.sents(query.countries);
    std::vector<Sentence> candidate_sents;
    for(auto uid : uids) candidate_sents.push_back(db.uid2sent[uid]);
    if(query.countries.size()==0) candidate_sents=db.sents;

    return candidate_sents;
}

}//data::ygp
}//data
