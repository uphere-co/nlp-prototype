#pragma once
#include "json/json.hpp"
#include <string>

namespace util{

using json_t = nlohmann::json;
json_t  load_json(std::string filename);

auto get_string_val=[](json_t const &json, std::string field)->std::string {
    return json[field];
};

}//namespace util
