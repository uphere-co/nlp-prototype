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
    json_t* make_input    ( int n, char*   str        );
    void    query_init    ( char*   configfile );
    json_t* query         ( json_t* input      );
    void    query_finalize( void               );
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
    //json_t input;
    
    ss << s;
    ss >> (*input);

    std::cout << input->dump(4) << std::endl;

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
    std::cout << answer->dump(4) << std::endl;
    return answer;  
}

void query_finalize( void )
{
    delete engine;
}

