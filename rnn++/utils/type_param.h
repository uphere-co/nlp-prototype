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


template<typename T>
constexpr DataType to_datatype();

template<>
constexpr DataType to_datatype<float>(){return DataType::sp;}
template<>
constexpr DataType to_datatype<double>(){return DataType::dp;}
template<>
constexpr DataType to_datatype<char>(){return DataType::chr;}
template<>
constexpr DataType to_datatype<int32_t>(){return DataType::i32;}
template<>
constexpr DataType to_datatype<int64_t>(){return DataType::i64;}
template<>
constexpr DataType to_datatype<uint32_t>(){return DataType::ui32;}
template<>
constexpr DataType to_datatype<uint64_t>(){return DataType::ui64;}


DataType datatype_from_string(std::string const &option);
std::string datatype_to_string(DataType type);
}//namespace util