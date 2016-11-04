#include "utils/json.h"

#include <fstream>

namespace util{

json_t  load_json(std::string filename){
    json_t  j;
    std::ifstream jsonData(filename, std::ifstream::in);
    if(jsonData.is_open()) {
        jsonData >> j;
    }
    return j;
}

}//namespace util
