#pragma once

#include <vector>
#include "utils/json.h"
#include "data_source/db_query.h"

namespace engine{

util::json_t to_json(std::vector<data::PerSentQueryResult> const &results);
void annotate_input_info(util::json_t &answer, data::QuerySentInfo const &info);
util::json_t to_json(std::vector<data::QueryResult> const &answers);

}//namespace engine
