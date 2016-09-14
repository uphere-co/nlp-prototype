#pragma once
#include "json/json.hpp"
#include <string>

namespace util{

nlohmann::json load_json(std::string filename);

}//namespace util
