#pragma once

#include <string>

#include "utils/json.h"

namespace data {
namespace ygp {

int dump_column(std::string table, std::string column, std::string index_col, std::string dump_path="");
void dump_psql(std::string cols_to_exports, std::string dump_path);

void dump_country_name(std::string table,
                       std::string country_code_col,
                       std::string index_col,
                       std::string dump_path);
void generate_country_columns(std::string dump_path, std::string country_columns_file);

void write_column_indexes(std::string output_filename,
                          std::string cols_to_exports,
                          std::vector<std::string> corenlp_outputs);

}//namespace data::ygp
}//namespace data
