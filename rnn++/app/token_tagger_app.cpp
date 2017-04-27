#include <iostream>

#include <fmt/printf.h>

#include "utils/json.h"
#include "tagger/token_tagger.h"
#include "wordrep/wikientity_base.h"
#include "wiki/wikidata.h"

#define BACKWARD_HAS_DW 1
#include <backward.hpp>
namespace backward {
backward::SignalHandling sh;
} // namespace backward


wordrep::wiki::SortedEntities create_entities(wordrep::WordUIDindex const& wordUIDs, std::string const& filename){
    tbb::concurrent_vector<wordrep::wiki::Entity> items;
    auto lines = util::string::readlines(filename);
    for(auto& line : lines)
        items.push_back({wordUIDs, line});
    tbb::parallel_sort(items.begin(), items.end());
    return {std::move(items)};
}

 
wordrep::WikidataUIDindex create_WikidataUIDindex(std::string const& filename) {
    tbb::concurrent_vector<std::pair<wordrep::WikidataUID,std::string>> items;
    
    auto lines = util::string::readlines(filename);
    for(auto& line : lines) {
        auto tokens = util::string::split(line, "\t");
        auto uid_str = tokens[0];
        auto uid = wordrep::WikidataUIDindex::get_uid(uid_str);
        items.push_back(std::make_pair(uid,uid_str));
    }
    tbb::parallel_sort(items.begin(), items.end(), [](auto& x, auto& y){
        return x.first<y.first;
    });
    return {std::move(items)};
}


wordrep::wiki::UIDSortedEntities create_uid_sorted_entities(wordrep::wiki::SortedEntities const & entities ) {
    tbb::concurrent_vector<wordrep::wiki::Entity> items;

    for(auto e = entities.cbegin(); e != entities.cend() ; e++ ) {
        items.push_back(*e);
    }
    return {std::move(items)};
}


int main( int argc, char** argv )
{
    std::cout << "start token_tagger_app" << std::endl;
//    util::json_t p = util::load_json("../tests/data/sentence.1.corenlp");
//    wordrep::WordUIDindex wordUIDs{"/data/groups/uphere/engine.rss/words.uid"};
    util::json_t p = util::load_json("../rnn++/tests/data/sentence.1.corenlp");
    wordrep::WordUIDindex wordUIDs{"/data/groups/uphere/engine.rss/all_words"};
    wordrep::wiki::SortedEntities entities = create_entities(wordUIDs, "F7745.all_entities");
    wordrep::wiki::UIDSortedEntities uid_sorted_ett = create_uid_sorted_entities(entities);
    wordrep::wiki::EntityReprs entity_reprs{uid_sorted_ett};
    wordrep::WikidataUIDindex wikidataUIDs = create_WikidataUIDindex("F7745.all_entities");   
    wikidata::GreedyAnnotator ann(entities);

    // int s = 0; 
     for(auto e = entities.cbegin(); e != entities.cend() ; e++ ) {
         std::cout << e->repr(wikidataUIDs,wordUIDs) << std::endl;
    }
    
    std::cout << p.dump(4) << std::endl;
    // std::cout << wordrep::WordUIDindex::get_uid("Earlier") << std::endl;
    //wordrep::AnnotatedSentence asent = tagger::test_program(ann,p);
    wordrep::DepParsedTokens tokens;
    data::CoreNLPjson c{p} ;
    tokens.append_corenlp_output(c);
    wordrep::Sentence sent ( 0, 1, 5 , &tokens) ;
    // wordrep::WordUIDindex widx("/data/groups/uphere/engine.rss/words.uid" ) ;
    // std::cout << sent.repr(widx)  << std::endl;

    auto asent = ann.annotate(sent);

    int s = 0;
    for( auto e = asent.begin() ; e != asent.end() ; ++e ) {
        s++;
        std::cout << s << std::endl;
    }
    std::cout << asent.sent.repr(wordUIDs) << std::endl;
    std::cout << asent.repr(entity_reprs,wikidataUIDs,wordUIDs) << std::endl;
}
