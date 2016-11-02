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
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -flto -ftree-vectorize")
message(${CMAKE_CXX_FLAGS})

find_package (Threads)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/src/)

FILE(GLOB rnn_files src/common/*cpp src/models/*cpp src/*cpp)
FILE(GLOB rnn_simple_model_files src/parser/*cpp src/*cpp)
FILE(GLOB simple_model_parsers src/*cpp
    src/parser/parser.cpp src/parser/voca.cpp src/parser/wordvec.cpp
    src/parser/compute.cpp src/parser/config.cpp src/parser/node.cpp src/parser/param.cpp)

FILE(GLOB wordrep_files src/wordrep/*cpp)
FILE(GLOB similarity_files src/similarity/*cpp)

FILE(GLOB util_files utils/*cpp)

add_executable(train_model0 app/train_model0 tests/test_simple_model
               ${rnn_simple_model_files} ${util_files})
#https://www.hdfgroup.org/HDF5/Tutor/compile.html HDF5 compile.
target_link_libraries(train_model0 hdf5_cpp hdf5_hl_cpp tbb lbfgs)

add_executable(train_model1 app/train_model1 tests/test_simple_model
               ${rnn_simple_model_files} ${util_files})
target_link_libraries(train_model1 hdf5_cpp hdf5_hl_cpp tbb lbfgs)

add_executable(train_model2 app/train_model2 tests/test_context_rnn
               ${rnn_files} ${rnn_simple_model_files} ${util_files})
target_link_libraries(train_model2 hdf5_cpp hdf5_hl_cpp tbb lbfgs fmt)

add_executable(train_model4 app/train_model4 tests/test_dp_merging
               ${rnn_simple_model_files} ${util_files})
target_link_libraries(train_model4 hdf5_cpp hdf5_hl_cpp tbb lbfgs)


add_executable(parser_model1 app/parser_model1
               ${simple_model_parsers} ${util_files})
target_link_libraries(parser_model1 hdf5_cpp hdf5_hl_cpp tbb)
add_executable(parser_model4 app/parser_model4  tests/test_voca_wordvec
              ${simple_model_parsers} ${util_files})
target_link_libraries(parser_model4 hdf5_cpp hdf5_hl_cpp tbb fmt)

add_executable(parser_crnn app/parser_crnn tests/test_context_rnn
        ${rnn_files} ${rnn_simple_model_files} ${util_files})
target_link_libraries(parser_crnn hdf5_cpp hdf5_hl_cpp tbb lbfgs fmt)


add_executable(phrases_model4 app/phrases_model4  tests/test_voca_wordvec
               ${simple_model_parsers} ${util_files})
target_link_libraries(phrases_model4 hdf5_cpp hdf5_hl_cpp tbb fmt)

add_executable(parser_similarity app/parser_eval tests/test_tree_spans
               utils/string)

add_executable(nndep_reader app/nndep_reader ${simple_model_parsers}
               ${similarity_files} ${wordrep_files} ${util_files})
target_link_libraries(nndep_reader hdf5_cpp hdf5_hl_cpp tbb fmt pq pqxx)


add_executable(hdf5io app/hdf5io src/parser/config src/parser/param utils/hdf5 utils/string src/wordrep/word_prob.h src/wordrep/word_prob.cpp)
target_link_libraries(hdf5io hdf5_cpp hdf5_hl_cpp tbb)


#add_executable(gsl boilerplate/gsl)
#add_executable(string boilerplate/string)
#add_executable(math boilerplate/math)
#add_executable(bfgs boilerplate/lbfgs)
#add_executable(regex boilerplate/regex)
#target_link_libraries(bfgs lbfgs)

add_library(parser STATIC ${simple_model_parsers} ${util_files})
target_link_libraries(parser fmt tbb hd5f_cpp)  # not effective when making query package

add_library(similarity STATIC ${similarity_files} ${wordrep_files}  ${util_files})
target_link_libraries(similarity fmt tbb hd5f_cpp pqxx) # not effective when making query package

add_library(util STATIC ${util_files})
#target_link_libraries(util hdf5_cpp hdf5_hl_cpp tbb)

install(TARGETS train_model0 train_model1 train_model4 parser_model1 parser_model4 phrases_model4 parser_similarity RUNTIME DESTINATION bin)

install(TARGETS similarity DESTINATION lib) 

install(TARGETS util DESTINATION lib) 

install(TARGETS parser DESTINATION lib) 



install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/src/parser" DESTINATION include)
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/utils" DESTINATION include)
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/src/similarity" DESTINATION include)
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/src/wordrep" DESTINATION include)

install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/src/dataset.h" DESTINATION include)
