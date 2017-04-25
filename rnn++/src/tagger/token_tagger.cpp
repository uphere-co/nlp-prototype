#include "tagger/token_tagger.h"

namespace tagger {

    void test_program( util::json_t const &p ) { 
        wordrep::DepParsedTokens tokens;
        data::CoreNLPjson c(p) ;
        tokens.append_corenlp_output(c);
        wordrep::Sentence sent ( 0, 1, 5 , &tokens) ;
        // auto i = wordrep::WordUIDindex::get_uid("Earlier");
        wordrep::WordUIDindex widx("/data/groups/uphere/engine.rss/words.uid" ) ;
        std::cout << sent.repr(widx)  << std::endl;
        
        return;
    }

}
