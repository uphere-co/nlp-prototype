#include "data_source/ygp_db.h"

#include <pqxx/pqxx>
#include <fmt/printf.h>

#include "utils/hdf5.h"
#include "utils/string.h"

using namespace util::io;

namespace data {
namespace ygp {

YGPindexer::YGPindexer(util::io::H5file const &file, std::string prefix)
        : chunk2idx{util::deserialize<RowIndex>(file.getRawData<int64_t>(H5name{prefix+".chunk2row_idx"}))},
          chunk2row_uid{util::deserialize<RowUID>(file.getRawData<int64_t>(H5name{prefix+".chunk2row"}))},
          chunk2col_uid{util::deserialize<ColumnUID>(file.getRawData<int64_t>(H5name{prefix+".chunk2col"}))}
{
    auto n = chunk2idx.size();
    assert(chunk2row_uid.size()==n);
    assert(chunk2col_uid.size()==n);
    //for(decltype(n)i=0; i!=n; ++i) {
    for(auto it=chunk2idx.cbegin(); it!=chunk2idx.cend(); ){
        auto i = std::distance(chunk2idx.cbegin(), it);
        auto row_idx=*it;
        auto row_uid=chunk2row_uid[i];
        auto col_uid=chunk2col_uid[i];
        map_to_uid[{col_uid,row_idx}]=row_uid;
        it = std::find_if_not(it, chunk2idx.cend(), [it](auto x){return x==*it;});
    }
}

CountryCodeAnnotator::CountryCodeAnnotator(std::string country_list){
    auto countries = util::string::readlines(country_list);
    for(auto const& country : countries) codes[country].push_back(country);
    codes["Korea"].push_back("South Korea");
    codes["China"].push_back("Hong Kong");
}
std::vector<std::string> CountryCodeAnnotator::tag(std::string content) const{
    std::vector<std::string> countries;
    for(auto const& it : codes){
        auto n = content.find(it.first);
        if(n==decltype(content)::npos) continue;
        util::append(countries, it.second);
    }
    return countries;
}

DBbyCountry::DBbyCountry(util::io::H5file const &file, std::string country_list){
    auto countries =util::string::readlines(country_list);
    for(auto country : countries) {
        auto rows=util::deserialize<RowUID>(file.getRawData<int64_t>(H5name{country+".row_uid"}));
        auto sents=util::deserialize<wordrep::SentUID>(file.getRawData<int64_t>(H5name{country+".sent_uid"}));
        rows_by_country[country]=rows;
        sents_by_country[country]=sents;
    }
}

YGPdb::YGPdb(std::string column_uids){
    auto lines = util::string::readlines(column_uids);
    assert(ColumnUID{}==ColumnUID{0});
    for(auto line : lines){
        auto cols = util::string::split(line, ".");
        tables.push_back(cols[0]);
        columns.push_back(cols[1]);
        index_cols.push_back(cols[2]);
    }
}

std::string YGPdb::raw_text(ColumnUID col_uid, RowIndex idx) const{
    pqxx::connection C{"dbname=C291145_gbi_test host=bill.uphere.he"};
    pqxx::work W(C);
    auto query=fmt::format("SELECT {} FROM {} where {}={};",
                           column(col_uid), table(col_uid), index_col(col_uid), idx.val);
    auto body= W.exec(query);
    W.commit();
    return body[0][0].c_str();
}

void annotation_on_result(util::json_t const &config, util::json_t &answers){
    YGPdb ygpdb{config["column_uids_dump"].get<std::string>()};
    for(auto &answer : answers){
        //answer["result_sent_uid"].push_back(sent.uid.val);
        //answer["result_row_uid"].push_back(row_uid.val);
        auto col_uids = answer["result_column_uid"];
        auto row_idxs = answer["result_row_idx"];
        auto offsets = answer["result_offset"];
        auto n = col_uids.size();
        for(decltype(n)i=0; i!=n; ++i){
            ColumnUID col_uid{col_uids[i].get<ColumnUID::val_t>()};
            RowIndex  row_idx{row_idxs[i].get<RowIndex::val_t>()};
            auto offset_beg = offsets[i][0].get<int64_t>();
            auto offset_end = offsets[i][1].get<int64_t>();

            auto row_str = ygpdb.raw_text(col_uid, row_idx);
            auto substr = util::string::substring_unicode_offset(row_str, offset_beg, offset_end);
            answer["result_DEBUG"].push_back(substr);
            answer["result_row_DEBUG"].push_back(row_str);
        }
    }
}

}//namespace data::ygp
}//namespace data
