#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include "similarity/similarity.h"

extern "C" {
    void query_init( char* configfile );
  void query( int fd /* char* queryfile */ );
    void query_finalize( void );
}

using json = nlohmann::json;
json config; 
SimilaritySearch* engine;

Timer timer{};


void query_init( char* configfile )
{
    std::cout << "fake init" << std::endl; 
    /*    config = load_json(configfile);
    engine = new SimilaritySearch(config);
    std::cout << config.dump(4) << std::endl;
    timer.here_then_reset("Search engine loaded.");    
    */
}

void query( int fd /* char* queryfile */  )
{
    std::cout << "fake query" << std::endl;

    char string[] = "hello world\n";
    write(fd, string, strlen(string)+1);
    /*
    std::cout << "query is called" << std::endl;
    auto input = load_json(queryfile);
    auto answer = engine->process_queries(input);
    timer.here_then_reset("Query is answered.");
    std::cout << answer.dump(4) << std::endl;
    */
}

void query_finalize( void )
{
    std::cout << "fake finalize" << std::endl;
    /*
    delete engine;
    */
}

