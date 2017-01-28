#include <iostream>

#include "enginewrapper.h"

using namespace std;
using namespace engine;
//using json = nlohmann::json;

EngineWrapper::EngineWrapper(const char* configfile)
        : config(util::load_json(configfile)), engine(new engine_t(config))
{
    std::cout << config.dump(4) << std:: endl;
    timer.here_then_reset("Search engine loaded.");
}

const char* serialize( util::json_t* j )
{
    stringstream ss;
    ss << j->dump(4);
    const std::string& str = ss.str();
    char* n_str = new char[str.size()+1];
    strcpy(n_str,str.c_str() );
    return n_str;
}

const char* find( util::json_t* j, const char* k )
{
    std::cerr << "find" << std::endl;    
    const std::string& str = (*j)[k];
    char* n_str = new char[str.size()+1];
    strcpy(n_str,str.c_str() );
    return n_str;
}


util::json_t* EngineWrapper::register_documents( const char* str, util::json_t* input )
{
    std::cerr << "register_documents" << std::endl;
    std::string query_str(str);
    (*input)["query_str"] = query_str;
    return (new util::json_t(engine->register_documents(*input)));
}

util::json_t* EngineWrapper::preprocess_query( util::json_t* input )
{
    std::cerr << "preprocess_query" << std::endl;        
    return (new util::json_t(engine->preprocess_query(*input)));
}

util::json_t* EngineWrapper::query( util::json_t* input )
{
    std::cerr << "query" << std::endl;            
    return (new util::json_t(engine->ask_query_stats(*input)));
}


util::json_t* EngineWrapper::suggest( util::json_t* input )
{
    return (new util::json_t(engine->ask_query_suggestion(*input)));
}



//void force_instantiation() {
//  EngineWrapper t("");
//}

