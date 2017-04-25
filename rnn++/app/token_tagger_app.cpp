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

 
// wordrep::wiki::wikidataUIDindex
wordrep::WikidataUIDindex create_WikidataUIDindex(std::string const& filename) {
    tbb::concurrent_vector<std::pair<wordrep::WikidataUID,std::string>> items;
    
    auto lines = util::string::readlines(filename);
    for(auto& line : lines) {
        auto tokens = util::string::split(line, "\t");
        auto uid = wordrep::WikidataUIDindex::get_uid(tokens[0]);
        items.push_back(std::make_pair(uid,tokens[1]));
    }
    return {std::move(items)};

    
    //return std::move(wikidataUIDs);   
}


int main( int argc, char** argv )
{
    std::cout << "start token_tagger_app" << std::endl;
    util::json_t p = util::load_json("../tests/data/sentence.1.corenlp");
    wordrep::WordUIDindex wordUIDs{"/data/groups/uphere/engine.rss/words.uid"};
    wordrep::wiki::SortedEntities entities = create_entities(wordUIDs, "F7745.all_entities");
    //wordrep::wiki::UIDSortedEntities uid_sorted_ett { entitie} ;
    wordrep::wiki::EntityReprs entity_reprs{entities.to_uid_sorted()};
    wordrep::WikidataUIDindex wikidataUIDs = create_WikidataUIDindex("F7745.all_entities");   
    wikidata::GreedyAnnotator ann(entities);

    // int s = 0; 
     for(auto e = entities.cbegin(); e != entities.cend() ; e++ ) {
        std::cout << e->repr(wikidataUIDs,wordUIDs) << std::endl;
    }
    
    std::cout << p.dump(4) << std::endl;
    // std::cout << wordrep::WordUIDindex::get_uid("Earlier") << std::endl;
    wordrep::AnnotatedSentence asent = tagger::test_program(ann,p);
    std::cout << asent.repr(entity_reprs,wikidataUIDs,wordUIDs) << std::endl;
    // asent( entity_reprs, , widx)
}
