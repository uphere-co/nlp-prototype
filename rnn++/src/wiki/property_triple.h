#pragma once
//TODO: move to .cpp file
#include <fstream>
#include <string>
#include <vector>

#include <fmt/printf.h>

#include "wordrep/word_uid.h"
#include "wordrep/annotated_sentence.h"
#include "wordrep/wikientity_repr.h"
#include "wordrep/wikientity.h"

#include "utils/string.h"
#include "utils/parallel.h"

namespace wikidata{

struct PropertyTriple{
    PropertyTriple(std::string line){
        auto tokens = util::string::split(line, "\t");
        if (tokens.size() != 3) {
            fmt::print("{}\n", line);
            assert(0);
        }
        entity = wordrep::WikidataUIDindex::get_uid(tokens[0]);
        property_type = wordrep::WikidataUIDindex::get_uid(tokens[1]);
        property = wordrep::WikidataUIDindex::get_uid(tokens[2]);
    }
    wordrep::WikidataUID entity;
    wordrep::WikidataUID property_type;
    wordrep::WikidataUID property;
};

struct PropertyTable{
    PropertyTable(std::string file){
        std::ifstream is{file};
        tbb::task_group g;
        tbb::concurrent_vector<PropertyTriple> items;
        while (auto buffer=util::string::read_chunk(is, 2000000)) {
            auto& chars =  buffer.value();
            g.run([&items, chars{std::move(chars)}](){
                std::stringstream ss;
                ss.str(chars.data());
                auto lines = util::string::readlines(std::move(ss));
                for(auto& line : lines)
                    items.push_back({line});
            });
        }
        g.wait();

        auto p31 = wordrep::WikidataUIDindex::get_uid("P31");
        for(PropertyTriple item : items){
            if(item.property_type == p31)
                instance_of[item.property].push_back(item.entity);
        }
        for(auto& x : instance_of)
            std::sort(x.second.begin(), x.second.end());
    }
    struct OpInstanceOf{
        bool is_instance(wordrep::WikidataUID uid) const {
            return util::isin(uids, uid);
        }
        std::vector<wordrep::WikidataUID> uids;
    };

    OpInstanceOf get_op_instance_of(wordrep::WikidataUID uid) const{
        auto x = instance_of.find(uid);
        return {x->second};
    }
    std::map<wordrep::WikidataUID,std::vector<wordrep::WikidataUID>> instance_of;
};

}//namespace wikidata
