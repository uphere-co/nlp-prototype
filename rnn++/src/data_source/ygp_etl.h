#pragma once

#include <string>

#include "utils/json.h"

namespace data {
namespace ygp {


int dump_column(std::string table, std::string column, std::string index_col);
void dump_psql(const char *cols_to_exports);

void write_country_code(util::json_t const &config);
void write_column_indexes(util::json_t const &config, std::string corenlp_outputs);

}//namespace data::ygp
}//namespace data
