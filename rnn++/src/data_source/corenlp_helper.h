#pragma once
#include <string>

#include "utils/json.h"

namespace data {

struct CoreNLPwebclient{
    CoreNLPwebclient(std::string script_path)
    : script_path{script_path} {}
    util::json_t from_query_file(std::string content_file_path) const;
    util::json_t from_query_content(std::string query_content) const;

    std::string script_path;
};

}//namespace data
