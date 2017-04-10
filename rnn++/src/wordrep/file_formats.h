#pragma once

#include "utils/flatbuffers/file_formats.h"

namespace wordrep{

struct TextFile{
    std::string name;
};
struct PythonScript{
    std::string name;
};
struct UIDIndexFile{
    std::string name;
};
struct AnnotatedTokenFile{
    AnnotatedTokenFile(std::string const& name, int n_block)
            : name{name}, n_block{n_block}
    {}
    std::string name;
    int n_block;
};
struct DepParsedFile{
    std::string name;
};

struct VocaIndexMapFile{
    util::io::I64Binary wuids;
};
struct WordEmbeggingFile{
    util::io::F32Binary mat;
};
struct WordImportanceFile{
    util::io::I64Binary uids;
    util::io::F32Binary scores;
};

struct WikiPropertyFile{
    std::string name;
};
struct WikiInstanceFile{
    std::string name;
};
struct WikiEntityByNameFile{
    std::string name;
};
struct WikiEntityByUIDFile{
    std::string name;
};

struct HDF5file{
    std::string name;
};

}//namespace wordrep;
