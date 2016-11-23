#pragma once

#include <map>
#include <string>

namespace data {
namespace ygp {

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

}//namespace data::ygp
}//namespace data
