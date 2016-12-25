#pragma once

#include "similarity/dep_similarity.h"
#include "utils/json.h"
#include "utils/profiling.h"

#include <iostream>

using namespace std;
using json = nlohmann::json;
using engine_t = engine::YGPQueryEngine;

const char* serialize( json* j );

class EngineWrapper {
    json config; 
    engine_t* engine0;
    util::Timer timer;
public:
    EngineWrapper(const char* configfile);
    json* register_documents( const char* str, json* input ); 
    json* query( json* input );
    
    ~EngineWrapper() { delete engine0; } 
};

