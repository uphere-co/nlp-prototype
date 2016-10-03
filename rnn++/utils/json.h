#pragma once
#include "json/json.hpp"
#include <string>

namespace util{

nlohmann::json load_json(std::string filename);

auto get_string_val=[](nlohmann::json const &json, std::string field)->std::string {
    return json[field];
};

}//namespace util
