#include "data_source/ygp_db.h"

#include <pqxx/pqxx>
#include <fmt/printf.h>

#include "utils/hdf5.h"
#include "utils/string.h"
#include "utils/versioned_name.h"

using namespace util::io;

namespace data {
namespace ygp {

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
        util::TypedPersistentVector<RowUID> rows{file, country+".row_uid"};
        util::TypedPersistentVector<wordrep::SentUID> sents{file, country+".sent_uid"};
        std::cerr<<"read " << country << " sents: " << sents.size()<<std::endl;
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
        full_names.push_back(line);
    }
}
bool YGPdb::is_in(std::string name) const{
    auto beg = full_names.cbegin();
    auto end = full_names.cend();
    return end!=std::find(beg, end, name);
}
ColumnUID YGPdb::col_uid(std::string name) const{
    auto beg = full_names.cbegin();
    auto it= std::find(beg, full_names.cend(), name);
    return it - beg;
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
