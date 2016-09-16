#include "utils/json.h"

#include <fstream>

using json = nlohmann::json;

namespace util{

json load_json(std::string filename){
    json j;
    std::ifstream jsonData(filename, std::ifstream::in);
    if(jsonData.is_open()) {
        jsonData >> j;
    }
    return j;
}

}//namespace util
