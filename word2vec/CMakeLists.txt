project(word2vec)
cmake_minimum_required(VERSION 3.4)

SET(HOME $ENV{HOME})
SET(CMAKE_C_COMPILER g)
SET(CMAKE_CXX_COMPILER g++)
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -march=native")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -gline-tables-only")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pedantic -Wall -Wextra")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Weverything -Wno-c++98-compat -Wno-c++98-compat-pedantic")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-float-equal")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}  -flto -ftree-vectorizer-verbose=5 -ftree-vectorize")
message(${CMAKE_CXX_FLAGS})

find_package (Threads)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
#include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../rnn++/)
include_directories(${RNNPP_PATH})

#include_directories(${HOME}/repo/srcc/GSL/include)

add_executable(voca_builder app/voca_builder  ${RNNPP_PATH}/utils/string.cpp ${RNNPP_PATH}/utils/hdf5.cpp)
target_link_libraries(voca_builder hdf5_cpp hdf5_hl_cpp)

add_executable(wordvec_trainer app/wordvec_trainer  ${RNNPP_PATH}/utils/string.cpp ${RNNPP_PATH}/utils/hdf5.cpp)
target_link_libraries(wordvec_trainer ${CMAKE_THREAD_LIBS_INIT} hdf5_cpp hdf5_hl_cpp)
