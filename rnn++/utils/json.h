#pragma once

#include <string>
#include <vector>
#include <map>
#include <cassert>

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

template<template<class> class TKEYS>
struct ConfigT{
    struct Key{
        std::string val;
        bool operator< (Key const& rhs) const {return val < rhs.val;}
    };
    ConfigT(util::json_t const& config)
            : keys(TKEYS<Key>{}.keys){
        for(auto key : keys) values[key] = util::get_str(config, key.val);
    }
    std::string value(std::string key) const {
        auto it = values.find(Key{key});
        assert(it!=values.cend());
        return it->second;
    }

    std::vector<Key> keys;
    std::map<Key,std::string> values;
};

}//namespace util
