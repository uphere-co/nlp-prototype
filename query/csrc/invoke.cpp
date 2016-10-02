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
    
    //json_t* input = new json_t;  // very dangerous here.
    json_t input;
    
    ss << str;
    ss >> input;

    std::cout << input.dump(4) << std::endl;




    auto answer = engine->process_queries(input);
    timer.here_then_reset("Query is answered.");
    // return &answer;
    std::cout << answer.dump(4) << std::endl;
    return NULL;  


    
    
    
    //return input;
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
    
//    std::cout << input->dump(4) << std::endl;
    //json_t input2(*input);


    json_t input2 = { { "phrase_store", "/data/groups/uphere/parsers/rnn_model4/phrases.h5"},
	              { "phrase_vec",  "news_wsj.text.vecs"},
		      { "phrase_word", "news_wsj.test.words"},
		      { "rnn_param_store", "/data/groups/uphere/data/groups/uphere/parsers/rnn_model4/rnn_params.h5"},
		      { "rnn_param_uid", "model4.d877053.2000" },
		      { "voca_name", "news_wsj.voca" },
		      { "w2vmodel_name", "news_wsj" },
		      { "wordvec_store", "/data/groups/uphere/parsers/rnn_model4/news_wsj.h5" },
	              { "queries", { "test", "this" } }};
    std::cout << input2["queries"] << std::endl;
    
    auto answer = engine->process_queries(input2);   // very dangerous here.
    //auto answer = engine->process_queries(*input);
    timer.here_then_reset("Query is answered.");
    // return &answer;
    std::cout << answer.dump(4) << std::endl;
    return NULL;  
}

void query_finalize( void )
{
    delete engine;
}

