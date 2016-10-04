#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
//
#include <iostream>
#include <fstream>
#include <sstream>
// 
#include "similarity/similarity.h"
#include "utils/json.h"
#include "template.hh"

using json_t = nlohmann::json;
using namespace std;

unique_ptr_wrapper_type(json_t)

extern "C" {
    
    json_t_p    json_create    ( char*  str      );
    void        json_finalize  ( json_t_p p      );
    const char* json_serialize ( json_t_p output );
    
    void        query_init     ( char*   configfile );
    json_t_p    query          ( json_t_p input     );
    void        query_finalize ( void               );
}

using json = nlohmann::json;
json config; 
SimilaritySearch* engine;

Timer timer{};

    
json_t_p json_create( char* str )
{
    auto input = make_unique<json_t>( json::parse(str) ) ;
    return new unique_ptr_wrapper<json_t>(input);
}

void json_finalize( json_t_p p )
{
    delete p;
}

const char* json_serialize( json_t_p p )
{
    json_t* output = p->get();
    stringstream ss;
    ss << output->dump(4);
    const std::string& str = ss.str();
    char* n_str= new char[str.size()+1];   // memory leak
    strcpy (n_str, str.c_str() ); 
    return n_str;
}

void query_init( char* configfile )
{
    config = load_json(configfile);
    engine = new SimilaritySearch(config);
    std::cout << config.dump(4) << std::endl;
    timer.here_then_reset("Search engine loaded."); 
}

json_t_p query( json_t_p input )
{
    auto answer = make_unique<json_t>( engine->process_queries(*(input->get()) )) ;
    timer.here_then_reset("Query is answered.");
    return new unique_ptr_wrapper<json_t>( answer ) ;
}

void query_finalize( void )
{
    delete engine;
}


