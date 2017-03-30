#pragma once
#include <string>

namespace data {
namespace ygp {

struct RowDumpFilePath{
    RowDumpFilePath(std::string path);
    std::string full_column_name() const;

    std::string table;
    std::string column;
    std::string index_col;
    int64_t index;
};

}//namespace data::ygp
}//namespace data
