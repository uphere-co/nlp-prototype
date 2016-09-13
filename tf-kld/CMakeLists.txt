project(tf-kld)
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
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -flto -ftree-vectorize")
#set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/build )
#set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib )

message(${CMAKE_CXX_FLAGS})

find_package (Threads)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../word2vec/)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../rnn++/)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../rnn++/src)
include_directories($ENV{MS_GSL}/GSL/include)
include_directories($ENV{SPDLOG}/spdlog/include)

add_executable(main app/main
               ../rnn++/src/parser/compute ../rnn++/src/parser/config ../rnn++/src/parser/node ../rnn++/src/parser/param 
               ../rnn++/src/parser/parser ../rnn++/src/parser/voca ../rnn++/src/parser/wordvec 
               ../rnn++/utils/string ../rnn++/utils/hdf5 ../rnn++/utils/math ../rnn++/utils/logger)
target_link_libraries(main hdf5_cpp hdf5_hl_cpp tbb armadillo openblas)

add_executable(MSPara app/MSPara
               ../rnn++/src/parser/compute ../rnn++/src/parser/config ../rnn++/src/parser/node ../rnn++/src/parser/param 
               ../rnn++/src/parser/parser ../rnn++/src/parser/voca ../rnn++/src/parser/wordvec 
               ../rnn++/utils/string ../rnn++/utils/hdf5 ../rnn++/utils/math ../rnn++/utils/logger)
target_link_libraries(MSPara hdf5_cpp hdf5_hl_cpp tbb armadillo openblas)


add_executable(pair_paraphrase app/pair_paraphrase
               ../rnn++/src/parser/compute ../rnn++/src/parser/config ../rnn++/src/parser/node ../rnn++/src/parser/param 
               ../rnn++/src/parser/parser ../rnn++/src/parser/voca ../rnn++/src/parser/wordvec 
               ../rnn++/utils/string ../rnn++/utils/hdf5 ../rnn++/utils/math ../rnn++/utils/logger)
target_link_libraries(pair_paraphrase hdf5_cpp hdf5_hl_cpp tbb armadillo openblas)
