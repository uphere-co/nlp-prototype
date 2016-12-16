#pragma once

#include <map>
#include <string>
#include <vector>
#include <utility>


#include "data_source/db.h"
#include "data_source/db_query.h"
#include "wordrep/dep_parsed.h"

#include "wordrep/indexes.h"

#include "utils/hdf5.h"
#include "utils/json.h"
#include "utils/base_types.h"
#include "utils/persistent_vector.h"

namespace data {
namespace ygp {

struct CountryCodeAnnotator{
    static std::string unknown() {return "All";}
    CountryCodeAnnotator(std::string country_list);
    std::vector<std::string> tag(std::string content) const;
private:
    std::map<std::string, std::vector<std::string>> codes;
};


struct DBbyCountry{
    DBbyCountry(util::io::H5file const &file, std::string country_list);
    std::vector<wordrep::SentUID> sents(std::string const& country) const {
        auto it=sents_by_country.find(country);
        if(it==sents_by_country.cend()) return {};
        return it->second.get();
    }
    std::vector<wordrep::SentUID> sents(std::vector<std::string> const& countries) const {
        std::vector<wordrep::SentUID> uids;
        for(auto country : countries) append(uids, sents(country));
        return uids;
    }
    std::string get_country(wordrep::SentUID uid) const{
        for(auto it : sents_by_country){
            auto country = it.first;
            for(auto suid : it.second.get()) if(uid==suid) return country;
        }
        return "Unknown";
    }
private:
    std::map<std::string, util::TypedPersistentVector<RowUID>> rows_by_country;
    std::map<std::string, util::TypedPersistentVector<wordrep::SentUID>> sents_by_country;
};

struct YGPdb{
    YGPdb(std::string column_uids);
    std::string table(ColumnUID idx) const {return tables[idx.val];}
    std::string index_col(ColumnUID idx) const {return index_cols[idx.val];}
    std::string column(ColumnUID idx) const {return columns[idx.val];}
    bool is_in(std::string name) const;
    ColumnUID col_uid(std::string name) const;
    ColumnUID beg() const {return ColumnUID{};}
    ColumnUID end() const {return ColumnUID::from_unsigned(tables.size());}
    std::string raw_text(ColumnUID col_uid, RowIndex idx) const;

    std::vector<std::string> tables;
    std::vector<std::string> columns;
    std::vector<std::string> index_cols;
    std::vector<std::string> full_names;
};

struct RowDumpFilePath{
    RowDumpFilePath(std::string path);
    std::string full_column_name() const;

    std::string table;
    std::string column;
    std::string index_col;
    int64_t index;
};

struct CountryColumn {
    CountryColumn() {
        table2country_code["reach_reports"] = "country_code";
        table2country_code["regulation"] = "countrycode";
        table2country_code["autchklist2"] = "countrycode";
    }

    std::string operator[](std::string table) const {
        auto it = table2country_code.find(table);
        if (it == table2country_code.cend()) return "";
        return it->second;
    }

    std::map<std::string, std::string> table2country_code;
};


void annotation_on_result(util::json_t const &config, util::json_t &answers);

}//namespace data::ygp
}//namespace data
