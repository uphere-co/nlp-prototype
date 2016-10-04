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

template<class T> 
class unique_ptr_wrapper {
private:
    unique_ptr<T> p_uniq; 
public:
    unique_ptr_wrapper( unique_ptr<T>& p ) { p_uniq = std::move(p); }
    T* get() { return p_uniq.get(); }
};

//typedef unique_ptr_wrapper<json_t>* json_p;

typedef unique_ptr_wrapper<json_t> unique_ptr_wrapper_json_t;
   // opaque pointer
//   typedef struct unique_ptr_wrapper_json_t unique_ptr_wrapper_json_t;
   typedef unique_ptr_wrapper_json_t* json_p;


extern "C" {
    
    json_p      json_create    ( char*  str    );
    void        json_finalize  ( json_p p      );
    const char* json_serialize ( json_p output );
    
    void        query_init     ( char*   configfile );
    json_p      query          ( json_p input      );
    void        query_finalize ( void               );
}

using json = nlohmann::json;
json config; 
SimilaritySearch* engine;

Timer timer{};

    
json_p json_create( char* str )
{
    unique_ptr<json_t> input( new json_t ) ;
    *(input.get()) = json::parse(str);
    
    return new unique_ptr_wrapper<json_t>(input);
    //return reinterpret_cast<json_p>(p1);
    //return p1;
}

void json_finalize( json_p p )
{
    delete p;
}

const char* json_serialize( json_p p )
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

json_p query( json_p input )
{
    unique_ptr<json_t> answer(new json_t );
    *(answer.get()) = engine->process_queries(*(input->get()));
    //unique_ptr<json_t> answer( engine->process_queries(*(input->get()) )) ;
    timer.here_then_reset("Query is answered.");
    return new unique_ptr_wrapper<json_t>( answer ) ;
}

void query_finalize( void )
{
    delete engine;
}


