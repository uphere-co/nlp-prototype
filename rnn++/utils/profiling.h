#pragma once
#include <chrono>

namespace util {

struct Timer{
    using time_t= std::chrono::time_point<std::chrono::high_resolution_clock>;
    void here(std::string mesg) const {
        time_t t_end = std::chrono::high_resolution_clock::now();
        std::cerr << mesg << " Wall time: "<< std::chrono::duration<double, std::milli>(t_end-t_start).count() << std::endl;
    }
    void reset(){t_start = std::chrono::high_resolution_clock::now();}
    void here_then_reset(std::string mesg) {here(mesg); reset();}
    time_t t_start = std::chrono::high_resolution_clock::now();
};


}//namespace util