#include <MacroPatternMatch.h>
#include <vector>
#include "Vector.h"

#include "query-bindingType.h"

#include "/home/wavewave/repo/srcp/nlp-prototype/rnn++/src/similarity/similarity.h"


using namespace engine;

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


void force_instantiation() {
  EngineWrapper t("");
}




