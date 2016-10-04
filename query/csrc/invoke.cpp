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
    void*   initialize     ( char*   str        );
    void    process        ( void* p );
    void    finalize       ( void* p );
    
    void    query_init     ( char*   configfile );
    json_t* query          ( json_t* input      );
    void    query_finalize ( void               );
    const char* get_output ( json_t* output );
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
 
    unique_ptr_wrapper( unique_ptr<T>& p ) {
	p_uniq = std::move(p);
    }
    
};
    
void* initialize( char* str )
{
    unique_ptr<myclass> p( new myclass ) ;
    unique_ptr_wrapper<myclass>* p1;
    p1 = new unique_ptr_wrapper<myclass>(p);
    
    
    return reinterpret_cast<void*>(p1);
   /*      
    unique_ptr<json_t,state_deleter&> p_json(new json_t,del); 

    unique_pointer* p;
    
    //json_t* input = new json_t;  // very dangerous here.
    json_t* input = p_json.get();
    cout << input->dump(4) << endl;
    *input = json::parse(str);

    p = new unique_pointer(p_json);
    //auto pp_json = std::move(p_json);

    //return reinterpret_cast<void*>(&pp_json);
    return p ; // std::move(p_json); */
}

void process (void * p )
{
    auto w = reinterpret_cast<unique_ptr_wrapper<myclass>* >(p);
    w->p_uniq.get()->quack();    
}

void finalize( void *p )
{
    auto w = reinterpret_cast<unique_ptr_wrapper<myclass>* >(p);
    
    delete w;
}


void query_init( char* configfile )
{
    
/*    config = load_json(configfile);
    engine = new SimilaritySearch(config);
    std::cout << config.dump(4) << std::endl;
    timer.here_then_reset("Search engine loaded.");      */
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

