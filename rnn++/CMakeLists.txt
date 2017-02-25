project(rnnpp)
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

FILE(GLOB etl_files src/data_source/*cpp)
FILE(GLOB wordrep_files src/wordrep/*cpp)
FILE(GLOB word2vec_files src/word2vec/*cpp)
FILE(GLOB similarity_files src/similarity/*cpp)
FILE(GLOB wiki_files src/wiki/*cpp)
FILE(GLOB util_files utils/*cpp)

FILE(GLOB test_files tests/*cpp)

add_executable(test_all app/test_all ${test_files}
        ${rnn_files} ${rnn_simple_model_files}
        ${similarity_files} ${wiki_files}
        ${etl_files} ${util_files} ${wordrep_files})
target_link_libraries(test_all stdc++fs hdf5_cpp hdf5_hl_cpp lbfgs tbb fmt pqxx dw)


add_executable(train_model0 app/train_model0 tests/test_simple_model
               ${rnn_simple_model_files} ${util_files})
#https://www.hdfgroup.org/HDF5/Tutor/compile.html HDF5 compile.
target_link_libraries(train_model0 stdc++fs hdf5_cpp hdf5_hl_cpp tbb lbfgs)

add_executable(train_model1 app/train_model1 tests/test_simple_model
               ${rnn_simple_model_files} ${util_files})
target_link_libraries(train_model1 stdc++fs hdf5_cpp hdf5_hl_cpp tbb lbfgs)

add_executable(train_model2 app/train_model2 tests/test_context_rnn
               ${rnn_files} ${rnn_simple_model_files} ${util_files})
target_link_libraries(train_model2 stdc++fs hdf5_cpp hdf5_hl_cpp tbb lbfgs fmt)

add_executable(train_model4 app/train_model4 tests/test_dp_merging
               ${rnn_simple_model_files} ${util_files})
target_link_libraries(train_model4 stdc++fs hdf5_cpp hdf5_hl_cpp tbb lbfgs)


add_executable(parser_model1 app/parser_model1
               ${simple_model_parsers} ${util_files})
target_link_libraries(parser_model1 stdc++fs hdf5_cpp hdf5_hl_cpp tbb)
add_executable(parser_model4 app/parser_model4  tests/test_voca_wordvec
              ${simple_model_parsers} ${util_files})
target_link_libraries(parser_model4 stdc++fs hdf5_cpp hdf5_hl_cpp tbb fmt)

add_executable(parser_crnn app/parser_crnn tests/test_context_rnn
        ${rnn_files} ${rnn_simple_model_files} ${util_files})
target_link_libraries(parser_crnn stdc++fs hdf5_cpp hdf5_hl_cpp tbb lbfgs fmt)


add_executable(phrases_model4 app/phrases_model4  tests/test_voca_wordvec
               ${simple_model_parsers} ${util_files})
target_link_libraries(phrases_model4 stdc++fs hdf5_cpp hdf5_hl_cpp tbb fmt)

add_executable(parser_similarity app/parser_eval tests/test_tree_spans
               utils/string)

add_executable(nndep_reader app/nndep_reader
        ${etl_files} ${similarity_files} ${wordrep_files} ${wiki_files} ${util_files})
target_link_libraries(nndep_reader stdc++fs hdf5_cpp hdf5_hl_cpp tbb fmt pqxx dw)
add_executable(stress_query_engine app/stress_query_engine
        ${etl_files} ${similarity_files} ${wordrep_files} ${wiki_files} ${util_files})
target_link_libraries(stress_query_engine stdc++fs hdf5_cpp hdf5_hl_cpp tbb fmt pqxx dw)

add_executable(ygpdb_etl app/ygpdb_etl
               ${etl_files} ${similarity_files} ${wordrep_files} ${wiki_files} ${util_files})
target_link_libraries(ygpdb_etl stdc++fs hdf5_cpp hdf5_hl_cpp tbb fmt pqxx dw)
add_executable(ygpdb_dump app/ygpdb_dump
        ${etl_files} ${similarity_files} ${wordrep_files} ${wiki_files} ${util_files})
target_link_libraries(ygpdb_dump stdc++fs hdf5_cpp hdf5_hl_cpp tbb fmt pqxx dw)
add_executable(word_count app/word_count
        ${etl_files} ${similarity_files} ${wordrep_files} ${wiki_files} ${util_files})
target_link_libraries(word_count stdc++fs hdf5_cpp hdf5_hl_cpp tbb fmt pqxx dw)
add_executable(word_count_collect app/word_count_collect
        ${etl_files} ${similarity_files} ${wordrep_files} ${wiki_files} ${util_files})
target_link_libraries(word_count_collect stdc++fs hdf5_cpp hdf5_hl_cpp tbb fmt pqxx dw)
add_executable(show_words_in_voca app/show_words_in_voca
        ${etl_files} ${similarity_files} ${wordrep_files} ${wiki_files} ${util_files})
target_link_libraries(show_words_in_voca stdc++fs hdf5_cpp hdf5_hl_cpp tbb fmt pqxx dw)
add_executable(word_importance_build app/word_importance_build
        ${etl_files} ${similarity_files} ${wordrep_files} ${wiki_files} ${util_files})
target_link_libraries(word_importance_build stdc++fs hdf5_cpp hdf5_hl_cpp tbb fmt pqxx dw)
add_executable(word_importance_export app/word_importance_export
        ${etl_files} ${similarity_files} ${wordrep_files} ${wiki_files} ${util_files})
target_link_libraries(word_importance_export stdc++fs hdf5_cpp hdf5_hl_cpp tbb fmt pqxx dw)

add_executable(word_context app/word_context
        ${wordrep_files} ${word2vec_files} ${util_files})
target_link_libraries(word_context stdc++fs hdf5_cpp hdf5_hl_cpp tbb fmt)
add_executable(word2vec_train app/word2vec_train
        ${util_files} ${wordrep_files} ${word2vec_files})
target_link_libraries(word2vec_train stdc++fs hdf5_cpp hdf5_hl_cpp tbb fmt)
add_executable(word2vec_eval app/word2vec_eval
        ${util_files} ${wordrep_files} ${word2vec_files})
target_link_libraries(word2vec_eval stdc++fs hdf5_cpp hdf5_hl_cpp tbb fmt)

add_executable(index_words app/index_words
               ${util_files} ${wordrep_files} ${word2vec_files})
target_link_libraries(index_words stdc++fs hdf5_cpp hdf5_hl_cpp tbb fmt)

add_executable(wikidata_annotator app/wikidata_annotator
        ${etl_files} ${similarity_files} ${wordrep_files} ${wiki_files} ${util_files})
target_link_libraries(wikidata_annotator stdc++fs hdf5_cpp hdf5_hl_cpp tbb fmt pqxx dw)
add_executable(wikidata_etl app/wikidata_etl
        ${etl_files} ${similarity_files} ${wordrep_files} ${wiki_files} ${util_files})
target_link_libraries(wikidata_etl stdc++fs hdf5_cpp hdf5_hl_cpp tbb fmt pqxx dw)


add_executable(hdf5io app/hdf5io src/parser/config src/parser/param  ${util_files} ${wordrep_files})
target_link_libraries(hdf5io stdc++fs hdf5_cpp hdf5_hl_cpp tbb fmt)

#add_executable(gsl boilerplate/gsl)
#add_executable(string boilerplate/string)
#add_executable(math boilerplate/math)
#add_executable(bfgs boilerplate/lbfgs)
#add_executable(regex boilerplate/regex)
#add_executable(json boilerplate/json)
#target_link_libraries(json fmt)
#target_link_libraries(bfgs lbfgs)

add_library(parser STATIC ${simple_model_parsers} ${util_files})
target_link_libraries(parser stdc++fs fmt tbb hd5f_cpp)  # not effective when making query package

# etl_files may need to be splitted out as a separate package.
add_library(similarity STATIC ${similarity_files} ${etl_files} ${wordrep_files} ${wiki_files}  ${util_files})
target_link_libraries(similarity stdc++fs fmt tbb hd5f_cpp pqxx dw) # not effective when making query package


add_library(util STATIC ${util_files})
#target_link_libraries(util hdf5_cpp hdf5_hl_cpp tbb)

add_library(enginewrapper STATIC wrapper/enginewrapper.cpp)
target_link_libraries(enginewrapper similarity parser tbb hd5f_cpp fmp pqxx stdc++ stdc++fs) # not effective when making query package

install(TARGETS train_model0 train_model1 train_model4 parser_model1 parser_model4 phrases_model4 parser_similarity RUNTIME DESTINATION bin)

install(TARGETS similarity DESTINATION lib) 

install(TARGETS util DESTINATION lib) 

install(TARGETS parser DESTINATION lib) 

install(TARGETS enginewrapper DESTINATION lib)

install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/src/parser" DESTINATION include)
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/utils" DESTINATION include)
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/src/similarity" DESTINATION include)
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/src/data_source" DESTINATION include)
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/src/wordrep" DESTINATION include)
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/src/wiki" DESTINATION include)

install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/src/dataset.h" DESTINATION include)
install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/wrapper/enginewrapper.h" DESTINATION include)
