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

std::string get_str(json_t const &json, std::string key){
    return json[key].template get<std::string>();
}
int64_t get_int(json_t const &json, std::string key){
    return json[key].template get<int64_t>();
}

}//namespace util
