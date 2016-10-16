#include "utils/random.h"

#include <random>
#include <sstream>

namespace util{

int64_t uuid_gen(){
    std::random_device rd{};
    std::mt19937 e{rd()};
    std::uniform_int_distribution<int64_t>  uniform_dist{1, 0xffffffff};
    return uniform_dist(e);
}

std::string get_uuid_str(){
    std::stringstream ss;
    ss << std::hex << uuid_gen()<<uuid_gen()<<uuid_gen();
    return ss.str();
}

}
