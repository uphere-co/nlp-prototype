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
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -flto -ftree-vectorize")
message(${CMAKE_CXX_FLAGS})

find_package (Threads)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${RNNPP_PATH}/src/)
include_directories(${RNNPP_PATH})

FILE(GLOB parser_files
    ${RNNPP_PATH}/src/parser/parser.cpp ${RNNPP_PATH}/src/parser/voca.cpp ${RNNPP_PATH}/src/parser/wordvec.cpp
    ${RNNPP_PATH}/src/parser/compute.cpp ${RNNPP_PATH}/src/parser/config.cpp ${RNNPP_PATH}/src/parser/node.cpp ${RNNPP_PATH}/src/parser/param.cpp)
FILE(GLOB util_files ${RNNPP_PATH}/utils/*cpp)

add_executable(trainer app/trainer wordrep/sentence2vec
               ${parser_files} ${util_files})
target_link_libraries(trainer hdf5_cpp hdf5_hl_cpp tbb)

add_executable(query_similarity app/query_similarity wordrep/sentence2vec
               ${parser_files} ${util_files})
target_link_libraries(query_similarity hdf5_cpp hdf5_hl_cpp tbb zmq similarity)

add_executable(model_evaluation app/model_evaluation wordrep/sentence2vec
               ${parser_files} ${util_files})
target_link_libraries(model_evaluation hdf5_cpp hdf5_hl_cpp tbb)
