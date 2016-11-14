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
#include "utils/profiling.h"
#include "template.hh"

using json_t = nlohmann::json;
using namespace std;

unique_ptr_wrapper_type(json_t)

extern "C" {
    json_t_p    json_create        ( char*   str         );
    void        json_finalize      ( json_t_p p          );
    const char* json_serialize     ( json_t_p output     );
    
    void        query_init         ( char*    configfile );
    json_t_p    register_documents ( char*    str, json_t_p input      ); 
    json_t_p    query              ( json_t_p input      );
    void        query_finalize     ( void                );
}

using json = nlohmann::json;
json config; 
engine::DepSimilaritySearch* engine0;

util::Timer timer{};

    
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
    char* n_str= new char[str.size()+1];   // memory leak (?). need to check
    strcpy (n_str, str.c_str() ); 
    return n_str;
}

void query_init( char* configfile )
{
    config = util::load_json(configfile);
    engine0 = new engine::DepSimilaritySearch(config);
    std::cout << config.dump(4) << std::endl;
    timer.here_then_reset("Search engine loaded."); 
}

json_t_p register_documents( char* str, json_t_p input )
{
    auto query_json = *(input->get());
    std::string query_str(str); 
    query_json["query_str"] = query_str;
    auto uids0 = engine0->register_documents(query_json);
    auto uids = make_unique<json_t>( uids0 );
    return new unique_ptr_wrapper<json_t>( uids ) ;
}

json_t_p query( json_t_p input )
{
    auto query_json = *(input->get());
    auto answer0 = engine0->ask_chain_query(query_json);
    auto answer = make_unique<json_t>( answer0 );
    timer.here_then_reset("Query is answered.");
    return new unique_ptr_wrapper<json_t>( answer ) ;
}

void query_finalize( void )
{
    delete engine0;
}


