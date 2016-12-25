#pragma once

#include "similarity/dep_similarity.h"
#include "utils/json.h"
#include "utils/profiling.h"

#include <iostream>

using namespace std;
using json = nlohmann::json;
using engine_t = engine::YGPQueryEngine;

/*
class JsonWrapper {
public:
    json content;
    JsonWrapper(const json& c ) { content = c; };
    JsonWrapper(const char* str);
    const char* serialize();
};
*/

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

