#pragma once

#include <string>

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
    std::string name;
};
struct DepParsedFile{
    std::string name;
};

struct VocaIndexMapFile{
    std::string name;
};
struct WordEmbeggingFile{
    std::string name;
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
