#include <iostream>

#include "utils/json.h"
#include "tagger/token_tagger.h"
#include "wordrep/wikientity_base.h"
#include "wiki/wikidata.h"


wordrep::wiki::SortedEntities create_entities(wordrep::WordUIDindex const& wordUIDs, std::string const& filename){
    tbb::concurrent_vector<wordrep::wiki::Entity> items;
    auto lines = util::string::readlines(filename);
    for(auto& line : lines)
        items.push_back({wordUIDs, line});
    tbb::parallel_sort(items.begin(), items.end());
    return {std::move(items)};
}


int main( int argc, char** argv )
{
    std::cout << "start token_tagger_app" << std::endl;
    util::json_t p = util::load_json("../tests/data/sentence.1.corenlp");
    wordrep::WordUIDindex wordUIDs("/data/groups/uphere/engine.rss/words.uid" ) ;
    wordrep::wiki::SortedEntities entities = create_entities(wordUIDs, "F7745.all_entities");
    wikidata::GreedyAnnotator ann(entities);

    
    std::cout << p.dump(4) << std::endl;
    std::cout << wordrep::WordUIDindex::get_uid("Earlier") << std::endl;
    wordrep::AnnotatedSentence asent = tagger::test_program(ann,p);
    // asent( entity_reprs, , widx)
}
