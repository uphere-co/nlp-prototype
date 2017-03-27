{ pkgs, toolz_cpp }:

with pkgs; 
[ cmake clang_38 
  boost
  hdf5 hdf5-cpp liblbfgs 
  tbb openblas  
  toolz_cpp.msgsl
  toolz_cpp.spdlog
  toolz_cpp.flatbuffers
  toolz_cpp.fmt
  toolz_cpp.json
  toolz_cpp.csv
  toolz_cpp.backwardcpp
  toolz_cpp.xxhashct
  toolz_cpp.variant
  elfutils
  cppzmq
  postgresql
  libpqxx
]

