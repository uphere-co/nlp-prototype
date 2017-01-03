#pragma once

#include "similarity/dep_similarity.h"
#include "utils/json.h"
#include "utils/profiling.h"


//class json_t; //forward declaration.

const char* serialize( util::json_t* j );

class EngineWrapper {
    using engine_t = engine::YGPQueryEngine;
    
    util::json_t config; 
    engine_t engine0;
    util::Timer timer;
public:
    EngineWrapper(const char* configfile);
    util::json_t* register_documents( const char* str, util::json_t* input ); 
    util::json_t* query( util::json_t* input );
    
    // ~EngineWrapper() { delete engine0; } 
};

