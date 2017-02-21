#include "src/wiki/wikidata.h"

#include <fstream>

#include <fmt/printf.h>

#include "wordrep/word_iter.h"
#include "wordrep/dep_parsed.h"

#include "utils/base_types.h"
#include "utils/parallel.h"
#include "utils/profiling.h"
#include "utils/algorithm.h"
#include "utils/string.h"

namespace{
using namespace wikidata;
template<typename TI>
AnnotatedSentence greedy_annotate(std::vector<Entity> const& entities, TI sent_beg, TI sent_end) {
    AnnotatedSentence out;
    auto to_reverse = [](auto it){return std::reverse_iterator<decltype(it)>{it};};
    size_t offset=0;
    size_t i = 0;
    auto beg = entities.cbegin();
    auto end = entities.cend();
    auto pbeg = beg;
    auto pend = end;

    while(true){
        auto t = *(sent_beg+offset+i);
        auto eq   = [t,i](Entity const& x){
            if(x.words.size()<=i) return false;
            return t==x.words[i];
        };
        auto less = [t,i](Entity const& x){
            if(x.words.size()<=i) return true;
            return t>x.words[i];
        };
        auto mit = util::binary_find(pbeg, pend, eq, less);
        if(!mit) {
            if(i == 0) {
                out.tokens.push_back({WordWithOffset{offset,1,t}});
                ++offset;
            } else {
                AmbiguousEntity entity;
                for(auto it=pbeg; it!=pend; ++it)
                    if(it->words.size()==i) entity.entities.push_back({offset,i,it->uid});
                if(!entity.entities.empty()) out.tokens.push_back({entity});
                offset += i;
                i = 0;
            }
            if(!(sent_beg + offset<sent_end)) break;
            pbeg = beg;
            pend = end;
            continue;
        }
        auto it = mit.value();
        pbeg = std::find_if_not(to_reverse(it), to_reverse(pbeg), eq).base();
        pend = std::find_if_not(it, pend, eq);
        ++i;
    }
    return out;
}

}//nameless namespace

namespace wikidata{

Entity::Entity(wordrep::WordUIDindex const& wordUIDs, std::string line) {
    auto tokens = util::string::split(line, "\t");
    if(tokens.size()!=2){
        fmt::print("{}\n", line);
        assert(0);
    }
    uid = wordrep::WikidataUIDindex::get_uid(tokens[0]);
    wordrep::WordIterBase<std::string> word_iter{tokens[1]};
    word_iter.iter([this,&wordUIDs](auto w){words.push_back(wordUIDs[w]);});
//        for(auto w : util::string::split(tokens[1], " "))

}

std::string Entity::repr(wordrep::WikidataUIDindex const& wikidataUIDs,
                 wordrep::WordUIDindex const& wordUIDs) const{
    std::stringstream ss;
    ss << wikidataUIDs[uid];
    for(auto word : words) ss << " " << wordUIDs[word];
    return ss.str();
}
std::ostream& operator<< (std::ostream& os, Entity const& a){
    fmt::print(os, "{}\t", a.uid);
    for(auto word: a.words) fmt::print(os, " {}", word);
    return os;
}

SortedEntities read_wikidata_entities(wordrep::WordUIDindex const& wordUIDs, std::istream&& is){
    tbb::task_group g;
    tbb::concurrent_vector<Entity> items;

    util::Timer timer;
    while (auto buffer=util::string::read_chunk(is, 2000000)) {
        auto& chars =  buffer.value();
        g.run([&wordUIDs, &items, chars{std::move(chars)}](){
            std::stringstream ss;
            ss.str(chars.data());
            auto lines = util::string::readlines(std::move(ss));
            for(auto& line : lines)
                items.push_back({wordUIDs, line});
        });
    }
    g.wait();
    timer.here_then_reset("Read all items.");
    tbb::parallel_sort(items.begin(), items.end());
    timer.here_then_reset("Sorted items.");
    std::vector<Entity> entities;
    for(auto&& item : items) entities.push_back(std::move(item));
    return SortedEntities{std::move(entities)};
}

SortedEntities read_wikidata_entities(wordrep::WordUIDindex const& wordUIDs, std::string entity_file){
    return read_wikidata_entities(wordUIDs, std::ifstream{entity_file});
}


Entity EntityReprs::operator[](wordrep::WikidataUID uid) const{
    auto it = reprs.find(uid);
    assert(it!=reprs.end());
    return {uid,it->second.front()};
}

std::vector<TaggedEntity> GreedyAnnotator::annotate(std::vector<wordrep::WordUID> const& text) const{
    auto tagged_sent = greedy_annotate(entities, text.begin(), text.end());
    std::vector<TaggedEntity> tagged;
    for(auto token : tagged_sent.tokens){
        token.token.match([](WordWithOffset w){},
                          [&tagged](AmbiguousEntity& w){
                              for(auto& entity : w.entities)
                                  tagged.push_back(entity);
                          });
    }
    return tagged;
}

AnnotatedSentence GreedyAnnotator::annotate(wordrep::Sentence const& sent) const{
    return greedy_annotate(entities, sent.iter_words().begin(), sent.iter_words().end());
}

std::vector<ConsecutiveTokens> is_contain(wordrep::Sentence const& sent,
                                          EntityReprs::OpEntityExactMatch const& op){
    std::vector<ConsecutiveTokens> offsets;
    auto iter_words = sent.iter_words();
    auto beg = iter_words.begin();
    auto end = iter_words.end();
    auto idx_beg=sent.front();
    for(auto it=beg; it!=end; ++it){
        auto n = op(it,end);
        if(n){
            offsets.push_back({idx_beg+(it-beg),n});
        }
    }
    return offsets;
}
std::vector<wordrep::WordPosition> head_word(wordrep::DepParsedTokens const& dict, ConsecutiveTokens words){
    auto positions = util::map(words,[&dict](auto idx){return dict.word_pos(idx);});
    std::vector<wordrep::WordPosition> heads;
    for(auto idx : words){
        auto mh = dict.head_pos(idx);
        if(!mh) continue;
        auto head = mh.value();
        if(util::isin(positions, head)) continue;
        heads.push_back(head);
    }
    return heads;
}

}//namespace wikidata
