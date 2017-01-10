#pragma once

// later we will move to c++17 std::optional. Now since fficxx-generated code
// supports up to c++14, I changed to naked pointer.
// #include <optional>

#include "similarity/query_engine.h"
#include "utils/json.h"
#include "utils/profiling.h"


//class json_t; //forward declaration.

#define YGPType 0
#define RSSType 1

const char* serialize( util::json_t* j );

class EngineWrapper {
    using ygp_engine_t = engine::YGPQueryEngine;
    using rss_engine_t = engine::RSSQueryEngine;
    int engine_type;  
    util::json_t config; 
    // std::optional<ygp_engine_t> ygp_engine;
    ygp_engine_t* ygp_engine = 0;
    // std::optional<rss_engine_t> rss_engine;
    rss_engine_t* rss_engine = 0;
    
    util::Timer timer;
public:
    EngineWrapper(int typ, const char* configfile);
    util::json_t* register_documents( const char* str, util::json_t* input ); 
    util::json_t* query( util::json_t* input );
    util::json_t* suggest( util::json_t* input );
    ~EngineWrapper() {
        if( ygp_engine ) delete ygp_engine;
        if( rss_engine ) delete rss_engine;
    }
};

