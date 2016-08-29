project(sentence2vec)
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
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}  -flto -ftree-vectorizer-verbose=5 -ftree-vectorize")
message(${CMAKE_CXX_FLAGS})

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../word2vec/)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../rnn++/)
include_directories($ENV{MS_GSL}/GSL/include)
include_directories($ENV{SPDLOG}/spdlog/include)

add_executable(trainer app/trainer 
               ../rnn++/parser/compute ../rnn++/parser/config ../rnn++/parser/node ../rnn++/parser/param 
               ../rnn++/parser/parser ../rnn++/parser/voca ../rnn++/parser/wordvec 
               ../rnn++/utils/string ../rnn++/utils/hdf5 ../rnn++/utils/math ../rnn++/utils/logger)
target_link_libraries(trainer hdf5_cpp hdf5_hl_cpp tbb)
