#pragma once

#include "similarity/dep_similarity.h"
#include "utils/json.h"
#include "utils/profiling.h"

#include <iostream>

using namespace std;
using json = nlohmann::json;
using engine_t = engine::YGPQueryEngine;

//template class engine::QueryEngine<data::ygp::DBInfo>;

class EngineWrapper {
    json config; 
    engine_t* engine0;
    util::Timer timer;
public:
    EngineWrapper(const char* configfile);

    ~EngineWrapper() { delete engine0; } 
};

class JsonWrapper {
    json content;
public:
    JsonWrapper(const char* str); 
}

