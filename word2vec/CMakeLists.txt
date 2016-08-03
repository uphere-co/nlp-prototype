project(rnnparser)
cmake_minimum_required(VERSION 3.4)

SET(HOME $ENV{HOME})
SET(CMAKE_C_COMPILER g)
SET(CMAKE_CXX_COMPILER g++)
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -march=native")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pedantic -Wall -Wextra")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Weverything -Wno-c++98-compat -Wno-c++98-compat-pedantic")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-float-equal")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}  -flto -ftree-vectorizer-verbose=5 -ftree-vectorize")
message(${CMAKE_CXX_FLAGS})

find_package (Threads)
#find_library(GFORTRAN_LIBRARY NAMES gfortran)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
#include_directories(${HOME}/repo/srcc/GSL/include)

add_executable(bh_tsne tsne utils sptree)
add_executable(grassmannian grassmannian)
target_link_libraries(grassmannian m pca armadillo openblas boost_iostreams boost_system boost_filesystem)

add_executable(distance distance utils.cpp)
target_link_libraries(distance m pca armadillo openblas boost_iostreams boost_system boost_filesystem)

add_executable(word2vec word2vec utils.cpp WordEmbed)
target_link_libraries(word2vec m pca armadillo ${GFORTRAN_LIBRARY} openblas boost_iostreams boost_system boost_filesystem)

add_executable(app_pca pca.cpp utils.cpp)
target_link_libraries(app_pca m pca armadillo ${GFORTRAN_LIBRARY} openblas boost_iostreams boost_system boost_filesystem)
