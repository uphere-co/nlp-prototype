#include <MacroPatternMatch.h>
#include <vector>
#include "Vector.h"

#include "query-bindingType.h"

#include "similarity/similarity.h"


using namespace engine;
using json = nlohmann::json;

Vector_instance_s(int)

EngineWrapper::EngineWrapper(const char* configfile) 
{
    config = util::load_json(configfile);
    engine0 = new engine_t(config);
    std::cout << config.dump(4) << std:: endl;
    timer.here_then_reset("Search engine loaded.");
}

const char* serialize( json* j )
{
    stringstream ss;
    ss << j->dump(4);
    const std::string& str = ss.str();
    char* n_str = new char[str.size()+1];
    strcpy(n_str,str.c_str() );
    return n_str;
}



json* EngineWrapper::query( json* input )
{
    return (new json(engine0->ask_chain_query(*input)));
}

json* EngineWrapper::register_documents( const char* str, json* input )
{
    std::string query_str(str);
    (*input)["query_str"] = query_str;
    return (new json(engine0->register_documents(*input)));
}


//void force_instantiation() {
//  EngineWrapper t("");
//}




