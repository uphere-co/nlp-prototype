#pragma once

#include <map>
#include <set>
#include <string>
#include <unordered_set>
#include <vector>

namespace tfkld{
namespace type{

    using int_t = int;
    using int64_t = int64_t;
    using real_t = float;
    using char_t = char;
}//namespace tfkld::type

using hashmap_t = std::map<type::int64_t, type::int_t>;
using vocab_t = std::map<std::string, type::int64_t>;
using tag_t = std::vector<std::string>;
using sen_t = std::vector<hashmap)t>;
using doc_t = std::vector<hashmap_t>;

}//namespace tfkld
