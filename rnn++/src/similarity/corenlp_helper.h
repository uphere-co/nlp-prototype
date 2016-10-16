#pragma once
#include <string>

#include "utils/json.h"

namespace engine {

struct CoreNLPwebclient{
    CoreNLPwebclient(std::string script_path)
    : script_path{script_path} {}
    nlohmann::json from_query_file(std::string content_file_path) const;
    nlohmann::json from_query_content(std::string query_content) const;

    std::string script_path;
};

}//namespace engine
