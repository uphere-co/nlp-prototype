project(sentence2vec)
cmake_minimum_required(VERSION 3.4)

SET(HOME $ENV{HOME})
SET(CMAKE_C_COMPILER clang)
SET(CMAKE_CXX_COMPILER clang++)
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++1z")
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -march=native")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -gline-tables-only")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fprofile-instr-generate")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pedantic -Wall -Wextra")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-deprecated-declarations")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-pass-failed")  #Disable warning for failing loop vectorization
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Weverything -Wno-c++98-compat -Wno-c++98-compat-pedantic")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-float-equal")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -flto -ftree-vectorize")
message(${CMAKE_CXX_FLAGS})

find_package (Threads)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${RNNPP_PATH}/src/)
include_directories(${RNNPP_PATH})
message("rnn++ path: " ${RNNPP_PATH})

FILE(GLOB parser_files ${RNNPP_PATH}/src/parser/* ${RNNPP_PATH}/src/*)
FILE(GLOB wordrep_files ${RNNPP_PATH}/src/wordrep/*cpp)
FILE(GLOB wiki_files ${RNNPP_PATH}/src/wiki/*cpp)
FILE(GLOB similarity_files ${RNNPP_PATH}/src/similarity/*cpp)
FILE(GLOB util_files ${RNNPP_PATH}/utils/*cpp)
FILE(GLOB etl_files ${RNNPP_PATH}/src/data_source/*cpp)


#add_executable(trainer app/trainer models/sentence2vec tests/test_sent2vec
#               ${parser_files} ${util_files} ${etl_files} ${wordrep_files})
#target_link_libraries(trainer hdf5_cpp hdf5_hl_cpp tbb fmt)

add_executable(query_similarity app/query_similarity
               ${similarity_files} ${wordrep_files} ${wiki_files} ${util_files} ${etl_files})
#target_link_libraries(query_similarity hdf5_cpp hdf5_hl_cpp tbb fmt zmq pq pqxx)
#target_link_libraries(query_similarity hdf5_cpp hdf5_hl_cpp tbb fmt zmq pqxx) #Link errors if includes `pq`
target_link_libraries(query_similarity stdc++fs hdf5_cpp hdf5_hl_cpp tbb fmt zmq pqxx dw)
#add_executable(model_evaluation app/model_evaluation models/sentence2vec
#               ${parser_files} ${util_files} ${wordrep_files})
#target_link_libraries(model_evaluation hdf5_cpp hdf5_hl_cpp tbb)

#add_executable(indexing_text app/indexing_text ${parser_files} ${util_files})
#target_link_libraries(indexing_text hdf5_cpp hdf5_hl_cpp tbb)
