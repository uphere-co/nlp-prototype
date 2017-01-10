#include <MacroPatternMatch.h>
#include <vector>
#include <iostream>
#include "Vector.h"

#include "query-bindingType.h"

#include "similarity/similarity.h"

using namespace std;
using namespace engine;
//using json = nlohmann::json;

Vector_instance_s(int)

EngineWrapper::EngineWrapper(int typ, const char* configfile) : engine_type(typ), config(util::load_json(configfile))
{
    if(engine_type == YGPType) {
        //ygp_engine_t e(config);  // c++17
        ygp_engine_t* e = new ygp_engine_t(config);
        ygp_engine = e;
    } else {
        //rss_engine_t e(config); // c++17
        rss_engine_t* e = new rss_engine_t(config);
        rss_engine = e;
    }
    
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



util::json_t* EngineWrapper::query( util::json_t* input )
{
    std::cout << (*input).dump(4) << std::endl;
    
    if( engine_type == YGPType ) { 
        // auto e = ygp_engine.value();                          // using c++17 optional
        // return (new util::json_t(e.ask_query_stats(*input)));
        return (new util::json_t(ygp_engine->ask_query_stats(*input)));        
    } else {
        std::cout << "inside EngineWrapper::query" << std::endl;
        // auto e = rss_engine.value();                          // using c++17 optional
        // return (new util::json_t(e.ask_query_stats(*input)));
        return (new util::json_t(rss_engine->ask_query_stats(*input)));        
    }
}

util::json_t* EngineWrapper::register_documents( const char* str, util::json_t* input )
{
    std::string query_str(str);
    (*input)["query_str"] = query_str;
    if( engine_type == YGPType ) { 
        // auto e = ygp_engine.value();
        // return (new util::json_t(e.register_documents(*input)));
        return (new util::json_t(ygp_engine->register_documents(*input)));        
    } else {
        std::cout << "inside EngineWrapper::register_documents" << std::endl;
        std::cout << (*input).dump(4) << std::endl;
        // auto e = rss_engine.value();
        // return (new util::json_t(engine0.register_documents(*input)));
        util::json_t* r = new util::json_t(rss_engine->register_documents(*input));
        std::cout << "after rss_engine->register_documents" << std::endl;
        return (r);
    }
}


//void force_instantiation() {
//  EngineWrapper t("");
//}




