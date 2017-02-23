#include "similarity/ygp.h"

#include <fmt/printf.h>

#include "similarity/dataset.h"

#include "utils/versioned_name.h"
#include "utils/hdf5.h"
#include "utils/math.h"
#include "utils/algorithm.h"

using util::io::h5read;
using wordrep::Sentence;
using wordrep::Sentences;
using engine::ScoredSentence;
using engine::plain_rank_cut;

namespace data{
namespace ygp{

YGPdb Factory::db() const {
    return {config.common.value("column_uids_dump")};
};

DBbyCountry Factory::db_by_country() const {
    return {h5read(util::get_latest_version(config.common.value("dep_parsed_store")).fullname),
            util::get_latest_version(config.ygp.value("country_uids_dump")).fullname};
}
CountryCodeAnnotator Factory::country_code_annotator() const {
    return {util::get_latest_version(config.ygp.value("country_uids_dump")).fullname};
}
std::vector<std::string> Factory::country_list() const{
    return util::string::readlines(util::get_latest_version(config.ygp.value("country_uids_dump")).fullname);
}

//It returns top N results per (col_uid) each.
std::vector<ScoredSentence> rank_cut_per_column(
        std::vector<ScoredSentence> const &relevant_sents,
        size_t n_max_per_table,
        DBIndexer const &ygp_indexer,
        ygp::YGPdb const &ygpdb){
    std::map<std::string, std::vector<ScoredSentence>> outputs_per_column;
    for(auto const &scored_sent : relevant_sents){
        auto const &sent = scored_sent.sent;
        auto col_uid=ygp_indexer.column_uid(sent.dict->chunk_idx(sent.front()));
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

//It returns top N results per (col_uid, row_idx) each.
std::vector<ScoredSentence> rank_cut_per_row_index(
        std::vector<ScoredSentence> const &relevant_sents,
        size_t n_max_per_table,
        DBIndexer const &ygp_indexer,
        ygp::YGPdb const &ygpdb){
    using ssents_per_rowidx = std::map<RowIndex,std::map<ColumnUID,std::vector<ScoredSentence>>>;
    std::map<std::string, ssents_per_rowidx> outputs_per_table_row_index;
    for(auto const &scored_sent : relevant_sents){
        auto const &sent = scored_sent.sent;
        auto cidx = sent.dict->chunk_idx(sent.front());
        auto col_uid = ygp_indexer.column_uid(cidx);
        auto table_name = ygpdb.table(col_uid);
        auto row_idx=ygp_indexer.row_idx(cidx);
        outputs_per_table_row_index[table_name][row_idx][col_uid].push_back(scored_sent);
        //keep_max_score_sent(outputs_per_table_row_index[table_name][row_idx], col_uid, scored_sent);
    }
    std::vector<ScoredSentence> top_N_results;
    for(auto const &pair : outputs_per_table_row_index){
        std::vector<std::vector<ScoredSentence>> sents;
        for(auto& x : pair.second){
            std::vector<ScoredSentence> sents_per_row_elm;
            for(auto &y : x.second){
                std::vector<ScoredSentence> const& ss = y.second;
                sents_per_row_elm.push_back(*util::max_element(ss,[](auto a,auto b){return a.score<b.score;}));
            }
            sents.push_back(sents_per_row_elm);
        }
        util::append(top_N_results, top_n_per_row_index(sents, n_max_per_table));
    }
    std::sort(top_N_results.begin(),top_N_results.end(), [](auto a,auto b){return a.score>b.score;});
    //return plain_rank_cut(top_N_results, n_max_per_table*2);
    return top_N_results;
}
DBInfo::DBInfo(Factory const& factory)
        : db{factory.db()},
          indexer{factory.common.db_indexer()},
          per_country{factory.db_by_country()},
          country_tagger{factory.country_code_annotator()}
{}

std::vector<Sentence> DBInfo::get_query_sents(
        DBInfo::query_t const& query,
        Sentences const &query_sent_uids,
        Sentences const &db_sent_uids) const{
    std::vector<Sentence> query_sents{};
    for(auto uid : query.uids){
        auto sent = query_sent_uids.find(uid);
        if(!sent) sent = db_sent_uids.find(uid);
        if(!sent) continue;
        query_sents.push_back(sent.value());
    }
    return query_sents;
}
std::vector<Sentence> DBInfo::get_candidate_sents(
        query_t const& query, engine::Dataset const& dataset) const{
    std::cerr<<"Find for a query in country DB of : ";
    for(auto const &country : query.countries) std::cerr<<country << ", ";
    std::cerr<<std::endl;
    if(query.countries.size()==0) std::cerr<<"No countries are specified. Find for all countries."<<std::endl;

    std::vector<ColumnUID> interested_columns;
    if(query.table_columns) {
        for(auto table_name : query.table_columns.value()){
            auto maybe_uid = db.table_column(table_name);
            if(!maybe_uid) continue;
            auto uid = maybe_uid.value();
            interested_columns.push_back(uid);
            fmt::print(std::cerr, "Table to search : {} {} \n", table_name, uid);

        }
    } else{
        fmt::print(std::cerr, "Find in all table\n");
    }

    auto uids = per_country.sents(query.countries);
    std::vector<Sentence> candidate_sents;
    for(auto uid : uids) candidate_sents.push_back(dataset.uid2sent[uid]);
    if(query.countries.size()==0) candidate_sents=dataset.sents;

    if(interested_columns.empty()) return candidate_sents;
    auto tmp= util::filter(candidate_sents,
                        [&interested_columns,this](auto& sent){
        return util::isin(interested_columns, indexer.column_uid(sent.dict->chunk_idx(sent.front())));
    });
    fmt::print(std::cerr, "Select {} among {} sents\n", tmp.size(), candidate_sents.size());
    return tmp;
}

}//data::ygp
}//data
