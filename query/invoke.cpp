#include "similarity/similarity.h"

extern "C" {
  void querytest( void );
}


void querytest( void )
{
    Timer timer{};
    auto config = load_json("config.json");
    SimilaritySearch engine{config};
    std::cout << config.dump(4) << std::endl;
    timer.here_then_reset("Search engine loaded.");    
    
    std::cout << "query test" << std::endl; 
}
