#include "utils/type_param.h"

#include <cassert>

namespace util{

DataType datatype_from_string(std::string const &option){
    if(option=="float32")       return DataType::sp;
    else if(option=="float64")  return DataType::dp;
    else if(option=="char")     return DataType::chr;
    else if(option=="int32_t")  return DataType::i32;
    else if(option=="int64_t")  return DataType::i64;
    else if(option=="uint32_t") return DataType::ui32;
    else if(option=="uint64_t") return DataType::ui64;
    else assert(0);
}
std::string datatype_to_string(DataType type){
    if(type==DataType::sp)        return "float32";
    else if(type==DataType::dp)   return "float64";
    else if(type==DataType::chr)  return "char";
    else if(type==DataType::i32)  return "int32_t";
    else if(type==DataType::i64)  return "int64_t";
    else if(type==DataType::ui32) return "uint32_t";
    else if(type==DataType::ui64) return "uint64_t";
    else assert(0);
}

}//namespace util
