#pragma once

#include <string>

#include "utils/json.h"

namespace data {
namespace ygp {


int dump_column(std::string table, std::string column, std::string index_col);
void dump_psql(const char *cols_to_exports);

void write_country_code(util::json_t const &config,
                        std::string cols_to_exports);

void parse_json_dumps(nlohmann::json const &config,
                      std::string cols_to_exports, int64_t n_max=-1);


}//namespace data::ygp
}//namespace data
