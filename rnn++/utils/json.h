#pragma once
#include "json/json.hpp"
#include <string>

namespace util{

using json_t = nlohmann::json;
json_t  load_json(std::string filename);


std::string get_str(json_t const &json, std::string key);
int64_t get_int(json_t const &json, std::string key);

}//namespace util
