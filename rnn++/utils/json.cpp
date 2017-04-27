#include "utils/json.h"
#include "utils/filesystem.h"

#include <exception>
#include <fstream>

namespace {

struct UnknownKeyException: public std::exception {
    UnknownKeyException(std::string key)
            :mesg{key+" : non-existing JSON key"}
    {}
    virtual const char* what() const throw() {
        return mesg.c_str();
    }
    std::string mesg;
};

}//

namespace util{

json_t  load_json(std::string filename){
    json_t  j;
    if(!util::file::is_exist(filename))
        throw std::runtime_error(filename+" is not found.");
    std::ifstream jsonData(filename, std::ifstream::in);
    assert(jsonData.is_open());
    jsonData >> j;
    return j;
}

std::string get_str(json_t const &json, std::string key){
    if(json.find(key)==json.end()) throw UnknownKeyException{key};
    return json[key].template get<std::string>();
}
int64_t get_int(json_t const &json, std::string key){
    return json[key].template get<int64_t>();
}

}//namespace util
