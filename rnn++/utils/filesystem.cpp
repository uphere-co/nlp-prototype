#include "utils/filesystem.h"

namespace util{
namespace file{

bool is_exist(std::string filename){
    std::ifstream f{filename};
    return f.good();
};

}//namespace util::file
}//namespace util

