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

add_executable(train_model0 app/train_model0 tests/test_simple_model
                   parser/compute parser/config parser/node parser/param 
                   parser/parser parser/voca parser/wordvec parser/optimizers
                   utils/string utils/hdf5 utils/math utils/logger)
#https://www.hdfgroup.org/HDF5/Tutor/compile.html HDF5 compile.
target_link_libraries(train_model0 hdf5_cpp hdf5_hl_cpp tbb lbfgs)

add_executable(train_model1 app/train_model1 tests/test_simple_model
                   parser/compute parser/config parser/node parser/param 
                   parser/parser parser/voca parser/wordvec parser/optimizers
                   utils/string utils/hdf5 utils/math utils/logger)
target_link_libraries(train_model1 hdf5_cpp hdf5_hl_cpp tbb lbfgs)

add_executable(train_model4 app/train_model4 tests/test_dp_merging
                   parser/compute parser/config parser/node parser/param 
                   parser/parser parser/voca parser/wordvec parser/optimizers
                   utils/string utils/hdf5 utils/math utils/logger)
target_link_libraries(train_model4 hdf5_cpp hdf5_hl_cpp tbb lbfgs)


add_executable(parser_model1 app/parser_model1 
               parser/compute parser/config parser/node parser/param 
               parser/parser parser/voca parser/wordvec 
               utils/hdf5 utils/math utils/string)
target_link_libraries(parser_model1 hdf5_cpp hdf5_hl_cpp tbb)
add_executable(parser_model4 app/parser_model4  tests/test_voca_wordvec
               parser/compute parser/config parser/node parser/param 
               parser/parser parser/voca parser/wordvec 
               utils/hdf5 utils/math utils/string)
target_link_libraries(parser_model4 hdf5_cpp hdf5_hl_cpp tbb fmt)

add_executable(parser_similarity app/parser_eval tests/test_tree_spans utils/string)

add_executable(hdf5io app/hdf5io parser/config parser/param utils/hdf5 utils/string)
target_link_libraries(hdf5io hdf5_cpp hdf5_hl_cpp tbb )


#add_executable(gsl boilerplate/gsl)
#add_executable(string boilerplate/string)
#add_executable(math boilerplate/math)
#add_executable(bfgs boilerplate/lbfgs)
#add_executable(regex boilerplate/regex)
#target_link_libraries(bfgs lbfgs)



install(TARGETS train_model0 train_model1 parser_model1 parser_model4 parser_similarity hdf5io RUNTIME DESTINATION bin)

install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/parser" DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}")
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/utils" DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}")
