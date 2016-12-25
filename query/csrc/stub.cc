#include <MacroPatternMatch.h>
#include <vector>
#include "Vector.h"

#include "query-bindingType.h"

#include "/home/wavewave/repo/srcp/nlp-prototype/rnn++/src/similarity/similarity.h"


using namespace engine;
using json = nlohmann::json;

//template QueryEngine<data::ygp::DBInfo>::QueryEngine(json);
//engine_t t; 

//EngineWrapper t("");
//EngineWrapper *t;

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

JsonWrapper* EngineWrapper::query( JsonWrapper* input )
{
    auto r = engine0->ask_chain_query(input->content);
    JsonWrapper* x = new JsonWrapper(r);
    return x;
}

void force_instantiation() {
  EngineWrapper t("");
}




