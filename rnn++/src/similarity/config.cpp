#include "similarity/config.h"

#include <ostream>

#include <fmt/printf.h>

namespace engine{

std::ostream& operator<< (std::ostream& os, Config::Key const& key){
    os << key.val;
    return os;
}
std::ostream& operator<< (std::ostream& os, Config const& config){
    for(auto key : config.values) fmt::print(std::cerr, "{:<25} : {}\n",key.first, key.second);
    return os;
}

}//namespace engine
