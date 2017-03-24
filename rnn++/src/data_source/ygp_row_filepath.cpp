#include "data_source/ygp_row_filepath.h"

#include "utils/string.h"
#include "utils/filesystem.h"

namespace data {
namespace ygp {

RowDumpFilePath::RowDumpFilePath(std::string path) {
    auto filename = util::file::get_filename(path);
    auto tokens = util::string::split(filename, ".");
    index     = std::stoi(tokens[3]);
    index_col = tokens[2];
    column    = tokens[1];
    table     = tokens[0];
}
std::string RowDumpFilePath::full_column_name() const{
    return util::string::join({table, column, index_col}, ".");
}

}//namespace data::ygp
}//namespace data
