project(rnnparser)
cmake_minimum_required(VERSION 3.4)

SET(HOME $ENV{HOME})
SET(CMAKE_C_COMPILER clang)
SET(CMAKE_CXX_COMPILER clang++)
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -march=native")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -gline-tables-only")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fprofile-instr-generate")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pedantic -Wall -Wextra")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Weverything -Wno-c++98-compat -Wno-c++98-compat-pedantic")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-float-equal")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}  -flto -ftree-vectorize")
message(${CMAKE_CXX_FLAGS})

find_package (Threads)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${HOME}/repo/srcc/GSL/include)
add_executable(app app/simple_parser parser/compute parser/config parser/node parser/param 
                   parser/parser parser/voca parser/wordvec 
                   utils/string utils/hdf5 utils/math tests/test_simple_model)
#https://www.hdfgroup.org/HDF5/Tutor/compile.html HDF5 compile.
target_link_libraries(app hdf5_cpp hdf5_hl_cpp tbb lbfgs)

add_executable(hdf5io app/hdf5io parser/config parser/param utils/hdf5 utils/string)
target_link_libraries(hdf5io hdf5_cpp hdf5_hl_cpp tbb )


#add_executable(gsl boilerplate/gsl)
#add_executable(string boilerplate/string)
#add_executable(math boilerplate/math)
#add_executable(bfgs boilerplate/lbfgs)
#target_link_libraries(bfgs lbfgs)
