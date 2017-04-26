#include <iostream>

#include "utils/json.h"
#include "tagger/token_tagger.h"

int main( int argc, char** argv )
{
    std::cout << "start token_tagger_app" << std::endl;
    util::json_t p = util::load_json("../tests/data/sentence.1.corenlp");

    std::cout << p.dump(4) << std::endl;
    std::cout << wordrep::WordUIDindex::get_uid("Earlier") << std::endl;
    tagger::test_program(p);
    
}
