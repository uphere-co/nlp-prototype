#pragma once
#include <string>

namespace util{

enum class DataType {
    sp,
    dp,
    chr,
    i32,
    i64,
    ui32,
    ui64,
};

util::DataType datatype_from_string(std::string const &option);

}//namespace util