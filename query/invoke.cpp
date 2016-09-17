#include "similarity/similarity.h"

extern "C" {
    void query_init( char* configfile );
    void query( char* queryfile );
    void query_finalize( void );
}

using json = nlohmann::json;
json config; 
SimilaritySearch* engine;

Timer timer{};


void query_init( char* configfile )
{
    config = load_json(configfile);// ("config.json");
    engine = new SimilaritySearch(config);
    std::cout << config.dump(4) << std::endl;
    timer.here_then_reset("Search engine loaded.");    
    
}

void query( char* queryfile )
{
    std::cout << "query is called" << std::endl;
    auto input = load_json(queryfile);
    auto answer = engine->process_queries(input);
    timer.here_then_reset("Query is answered.");
    std::cout << answer.dump(4) << std::endl;
}

void query_finalize( void )
{
    delete engine;
}

