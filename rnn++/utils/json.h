#pragma once

#include <string>

#include <json/json.hpp>

#include "utils/optional.h"

namespace util{

using json_t = nlohmann::json;
json_t  load_json(std::string filename);

template<typename T>
std::optional<T> find(json_t const &json, std::string key){
    if(json.find(key) != json.end()) return json[key].get<T>();
    return {};
}

std::string get_str(json_t const &json, std::string key);
int64_t get_int(json_t const &json, std::string key);

}//namespace util
