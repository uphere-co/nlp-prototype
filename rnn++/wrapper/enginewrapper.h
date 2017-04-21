#pragma once

// later we will move to c++17 std::optional. Now since fficxx-generated code
// supports up to c++14, I changed to naked pointer.
// #include <optional>

#include "similarity/query_engine.h"
#include "utils/json.h"
#include "utils/profiling.h"


//class json_t; //forward declaration.

const char* serialize( util::json_t* j );

const char* find( util::json_t* j, const char* k );

class EngineWrapper {
    using engine_t = engine::QueryEngine;
    util::json_t config;
    engine_t* engine;
    
    util::Timer timer;
public:
    EngineWrapper(const char* configfile);
    util::json_t* register_documents( const char* str, util::json_t* input );
    util::json_t* preprocess_query( util::json_t* input );
    util::json_t* query( util::json_t* input );
    ~EngineWrapper() {
        delete engine;
    }
};

