#include <iostream>

#include "utils/profiling.h"

namespace util {

void Timer::here(std::string mesg) const {
    time_t t_end = std::chrono::high_resolution_clock::now();
    std::cerr << mesg << " Wall time: " << std::chrono::duration<double, std::milli>(t_end - t_start).count()
              << std::endl;
}

}//namespace util