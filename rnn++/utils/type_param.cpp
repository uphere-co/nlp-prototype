#include "utils/type_param.h"

#include <cassert>

namespace util{

DataType datatype_from_string(std::string const &option){
    if(option=="float32") return util::DataType::sp;
    else if(option=="float64") return util::DataType::dp;
    else if(option=="char") return util::DataType::chr;
    else if(option=="int32_t") return util::DataType::i32;
    else if(option=="int64_t") return util::DataType::i64;
    else if(option=="uint32_t") return util::DataType::ui32;
    else if(option=="uint64_t") return util::DataType::ui64;
    else assert(0);
}

}//namespace util
