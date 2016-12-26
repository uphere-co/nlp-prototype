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

JsonWrapper::JsonWrapper(const char* str )
{
    content = json::parse( str ); 
}

const char* JsonWrapper::serialize()
{
    //  content;
    stringstream ss;
    ss << content.dump(4);
    const std::string& str = ss.str();
    char* n_str = new char[str.size()+1];
    strcpy(n_str,str.c_str() );
    return n_str;
}



JsonWrapper* EngineWrapper::query( JsonWrapper* input )
{
    auto r = engine0->ask_chain_query(input->content);
    return (new JsonWrapper(r));
}

JsonWrapper* EngineWrapper::register_documents( const char* str, JsonWrapper* input )
{
    std::string query_str(str);
    input->content["query_str"] = query_str;
    return (new JsonWrapper(engine0->register_documents(input->content)));
}


void force_instantiation() {
  EngineWrapper t("");
}




