#include "wiki/wikidata.h"

#include <sstream>
#include <fstream>

#include <fmt/printf.h>

#include "wordrep/dep_parsed.h"

#include "utils/base_types.h"
#include "utils/parallel.h"
#include "utils/profiling.h"
#include "utils/algorithm.h"
#include "utils/string.h"

namespace{
using namespace wikidata;
template<typename TE, typename TI>
std::vector<AnnotatedToken> greedy_annotate(TE beg, TE end, TI sent_beg, TI sent_end) {
    std::vector<AnnotatedToken> tokens;
    auto to_reverse = [](auto it){return std::reverse_iterator<decltype(it)>{it};};
    size_t offset=0;
    size_t i = 0;
    auto pbeg = beg;
    auto pend = end;

//    fmt::print(std::cerr, "Sent length : {}\n", sent_end-sent_beg);
    while(true){
        auto t = *(sent_beg+offset+i);
        auto min_len = i+1;
        auto eq   = [t,i,min_len](wordrep::wiki::Entity const& x){
            if(x.words.size()<min_len) return false;
            return t==x.words.uids[i];
        };
        auto less = [t,i,min_len](wordrep::wiki::Entity const& x){
            if(x.words.size()<min_len) return true;
            return t>x.words.uids[i];
        };
//        fmt::print(std::cerr, "offset = {}, i = {},   end:{} beg:{}\n", offset, i, pend-beg,pbeg-beg);
        auto mit = util::binary_find(pbeg, pend, eq, less);

//        bool is_it{mit};
//        fmt::print(std::cerr, "offset = {}, i = {}, {} end:{} beg:{}\n", offset, i, is_it, pend-beg,pbeg-beg);
        if(!mit) {
            if(i == 0) {
                tokens.push_back({WordWithOffset{offset,t}});
                ++offset;
            } else {
                wordrep::wiki::AmbiguousUID uid;
                for(auto it=pbeg; it!=pend; ++it)
                    if(it->words.size()==i) uid.candidates.push_back(it->uid);
                if(uid.candidates.empty()){
                    tokens.push_back({WordWithOffset{offset,t}});
                    ++offset;
                    i = 0;
                } else{
                    tokens.push_back({wordrep::wiki::AmbiguousEntity{offset,i,uid}});
                    offset += i;
                    i = 0;
                }
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
    return tokens;
}

}//nameless namespace

namespace wikidata{

std::string AnnotatedToken::repr(wordrep::wiki::EntityReprs const& entity_reprs,
                 wordrep::WikidataUIDindex const& wikidataUIDs,
                 wordrep::WordUIDindex const& wordUIDs) const {
    std::stringstream ss;
    val.match([&ss,&entity_reprs,&wordUIDs,&wikidataUIDs](wordrep::wiki::AmbiguousEntity w) {
        fmt::print(ss," (");
        for (auto uid : w.uid.candidates)
            fmt::print(ss,"{} ", entity_reprs[uid].repr(wikidataUIDs, wordUIDs));
        fmt::print(ss,")");
    },
    [&ss,&wordUIDs](WordWithOffset w) {
        fmt::print(ss,"{} ", wordUIDs[w.uid]);
    });
    return ss.str();
}


wordrep::wiki::SortedEntities read_wikidata_entities(wordrep::WordUIDindex const& wordUIDs, std::istream&& is){
    tbb::task_group g;
    tbb::concurrent_vector<wordrep::wiki::Entity> items;

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
    std::vector<wordrep::wiki::Entity> entities;
    for(auto&& item : items) entities.push_back(std::move(item));
    timer.here_then_reset("Build SortedEntities.");
    return {std::move(entities)};
}

wordrep::wiki::SortedEntities read_wikidata_entities(wordrep::WordUIDindex const& wordUIDs, std::string entity_file){
    return read_wikidata_entities(wordUIDs, std::ifstream{entity_file});
}




std::vector<TaggedEntity> GreedyAnnotator::annotate(std::vector<wordrep::WordUID> const& text) const{
    auto tokens = greedy_annotate(entities.cbegin(), entities.cend(), text.begin(), text.end());
//    fmt::print("Annotator returns {} tokens.", tokens.size());
    std::vector<TaggedEntity> tagged;
    for(auto token : tokens){
        token.val.match([](WordWithOffset /*w*/){
//                           fmt::print("Word token not tagged : {}\n", w.uid);
                        },
                          [&tagged](wordrep::wiki::AmbiguousEntity& w){
//                              fmt::print("Word token tagged. Offset : {}\n", w.offset);
                              for(auto& uid : w.uid.candidates)
                                  tagged.push_back({w.offset,w.len, uid});
                          });
    }
    return tagged;
}

wordrep::AnnotatedSentence GreedyAnnotator::annotate(wordrep::Sentence const& sent) const{
    auto tokens = greedy_annotate(entities.cbegin(), entities.cend(), sent.iter_words().begin(), sent.iter_words().end());

    wordrep::AnnotatedSentence annoted_sent = {sent, util::map(tokens, [&sent](auto& t){return t.to_sent_token(sent);})};
    return annoted_sent;
}

std::vector<wordrep::WordPosition> head_word_pos(wordrep::DepParsedTokens const& dict,
                                             wordrep::ConsecutiveTokens words){
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
