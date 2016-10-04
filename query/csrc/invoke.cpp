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
    //void* create_unique_ptr 
    // void* make_input     ( char*   str        );
    void*   json_create     ( char*   str        );
    //void    process        ( void* p );
    void    json_finalize       ( void* p );
    
    void    query_init     ( char*   configfile );
    void* query          ( void* input      );
    void    query_finalize ( void               );
    const char* get_output ( void* output );
}

using json = nlohmann::json;
json config; 
SimilaritySearch* engine;

Timer timer{};

class myclass {
public:
    myclass() { cout << "created" << endl; }
    ~myclass() { cout << "deleted" << endl; }
    void quack( void ) { cout << "quack" << endl; }
};

template<class T> 
struct unique_ptr_wrapper {
    unique_ptr<T> p_uniq; 

    unique_ptr_wrapper( T* p ) : p_uniq(p) {}
    
    unique_ptr_wrapper( unique_ptr<T>& p ) {
	p_uniq = std::move(p);
    }
    
};
    
void* json_create( char* str )
{
    json_t* input = new json_t; 
    *input = json::parse(str);
    
    unique_ptr_wrapper<json_t>* p1 = new unique_ptr_wrapper<json_t>(input);
    return reinterpret_cast<void*>(p1);
}

void json_finalize( void *p )
{
    auto w = reinterpret_cast<unique_ptr_wrapper<myclass>* >(p);
    cout << "finalize called" << endl;    
    delete w;
}


void query_init( char* configfile )
{
    config = load_json(configfile);
    engine = new SimilaritySearch(config);
    std::cout << config.dump(4) << std::endl;
    timer.here_then_reset("Search engine loaded."); 
}

void* query( void* input )
{
    auto w = reinterpret_cast<unique_ptr_wrapper<json_t>*>(input);
    cout << w->p_uniq.get()->dump(4) << endl;
    json_t *answer = new json_t;  
    *answer = engine->process_queries(*(w->p_uniq.get()));
    cout << answer->dump(4) << endl;
    timer.here_then_reset("Query is answered.");

    unique_ptr_wrapper<json_t> *p2 = new unique_ptr_wrapper<json_t>( answer ) ;
    return reinterpret_cast<void*>(p2);  
}

void query_finalize( void )
{
    delete engine;
}

const char* get_output( void* p )
{
    auto w = reinterpret_cast<unique_ptr_wrapper<json_t>*>(p);
    json_t* output = w->p_uniq.get();
    stringstream ss;
    ss << output->dump(4);
    const std::string& str = ss.str();
    char* n_str= new char[str.size()+1];   // memory leak
    strcpy (n_str, str.c_str() ); 
    return n_str;
}

