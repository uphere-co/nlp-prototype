#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
//
#include <iostream>
#include <fstream>
// 
#include "similarity/similarity.h"

using namespace std;


extern "C" {
    void query_init( char* configfile );
    void query( istream* is, ostream* os ); // int fq, int fr /* char* queryfile */ );
    void query_finalize( void );
}

using json = nlohmann::json;
json config; 
SimilaritySearch* engine;

Timer timer{};


void query_init( char* configfile )
{
  //std::cout << "fake init" << std::endl;
   
    config = load_json(configfile);
    engine = new SimilaritySearch(config);
    std::cout << config.dump(4) << std::endl;
    timer.here_then_reset("Search engine loaded.");     
}

//void query( int fq, int fr )
void query( istream* is, ostream* os )
{
  /* std::cout << "fake query" << std::endl;
  std::string str; 
  (*is) >> str ;
  std::cout << str << std::endl;
  (*os) << str << str << std::endl; */
  
    json input; 
    (*is) >> input ;
    std::cout << "j.size() = " << input.size() << std::endl;
    
    std::cout << "query is called" << std::endl;
    // auto input = load_json(queryfile);
    auto answer = engine->process_queries(input);
    timer.here_then_reset("Query is answered.");
    (*os) << answer.dump(4) << std::endl;
    delete os->rdbuf();
    delete os;
    // os->setstate(std::ios::eofbit);
    //delete os;
}

void query_finalize( void )
{
  //std::cout << "fake finalize" << std::endl;
      delete engine;
}

