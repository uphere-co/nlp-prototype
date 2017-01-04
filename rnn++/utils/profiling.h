#pragma once
#include <chrono>
#include <string>

namespace util {

struct Timer{
    using time_t= std::chrono::time_point<std::chrono::high_resolution_clock>;
    void here(std::string mesg) const;
    void reset(){t_start = std::chrono::high_resolution_clock::now();}
    void here_then_reset(std::string mesg) {here(mesg); reset();}
    time_t t_start = std::chrono::high_resolution_clock::now();
};

struct MockTimer{
    void here(std::string ) const {};
    void reset() const {}
    void here_then_reset(std::string) const {}
};


}//namespace util