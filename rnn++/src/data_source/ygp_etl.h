#pragma once

#include <string>

#include "utils/json.h"

namespace data {
namespace ygp {

int dump_column(std::string table, std::string column, std::string index_col, std::string dump_path="");
void dump_psql(std::string cols_to_exports, std::string dump_path);
int parse_column(std::string table, std::string column, std::string index_col);
void parse_psql(std::string cols_to_exports);

void write_column(std::vector<int64_t> rows, std::string filename,
                  std::string prefix, std::string colname);
void overwrite_column(std::vector<int64_t> rows, std::string filename,
                      std::string prefix, std::string colname);
void write_country_code(util::json_t const &config, int minor_version);
void write_column_indexes(std::string output_filename,
                          std::string cols_to_exports,
                          std::string prefix,
                          std::vector<std::string> corenlp_outputs);

}//namespace data::ygp
}//namespace data
