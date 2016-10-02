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

using json_t = nlohmann::json;
using namespace std;


extern "C" {
    json_t* make_input     ( int n, char*   str        );
    void    query_init     ( char*   configfile );
    json_t* query          ( json_t* input      );
    void    query_finalize ( void               );
    const char* get_output ( json_t* output );
}

using json = nlohmann::json;
json config; 
SimilaritySearch* engine;

Timer timer{};

json_t* make_input( int n, char* str )
{
    stringstream ss ;
    std::string s( str, n);
    json_t* input = new json_t;  // very dangerous here.
    ss << s;
    ss >> (*input);
    return input;
}

void query_init( char* configfile )
{
    config = load_json(configfile);
    engine = new SimilaritySearch(config);
    std::cout << config.dump(4) << std::endl;
    timer.here_then_reset("Search engine loaded.");     
}

json_t* query( json_t* input )
{
    json_t* answer = new json_t;                 // very dangerous: memory leak.
    *answer = engine->process_queries(*input);   // very dangerous here.
    timer.here_then_reset("Query is answered.");
    return answer;  
}

void query_finalize( void )
{
    delete engine;
}

const char* get_output( json_t* output )
{
    stringstream ss;
    ss << output->dump(4);
    const std::string& str = ss.str();
    char* n_str= new char[str.size()+1];   // memory leak
    strcpy (n_str, str.c_str() ); 
    return n_str;
}

