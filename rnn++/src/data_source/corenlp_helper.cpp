#include "data_source/corenlp_helper.h"

#include <iostream>
#include <sstream>
#include <fstream>
#include <stdlib.h>

#include "utils/random.h"


namespace data{

util::json_t CoreNLPwebclient::from_query_file(std::string content_file_path) const {
    std::string command = "python "+script_path + "  "+content_file_path;
    int ret=system(command.c_str());
    if(ret) std::cerr << "Non-zero return : " << command.c_str() << std::endl;
    return util::load_json(content_file_path+".corenlp");
}
util::json_t CoreNLPwebclient::from_query_content(std::string query_content) const {
    std::string content_file_path = "tmp."+util::get_uuid_str();
    std::ofstream temp_file;
    temp_file.open (content_file_path);
    temp_file << query_content;
    temp_file.close();
    auto query_json = from_query_file(content_file_path);
    auto commend_remove_temp_file = "rm -f "+content_file_path +"*";
    int ret = system(commend_remove_temp_file.c_str());
    if(ret) std::cerr << "Non-zero return : " << commend_remove_temp_file.c_str() << std::endl;
    return query_json;
}

}//namespace data
