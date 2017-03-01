#pragma once
//TODO: move to .cpp file
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_map>

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
        if (tokens.size() != 2) {
            fmt::print("{}\n", line);
            assert(0);
        }
        entity = wordrep::WikidataUIDindex::get_uid(tokens[0]);
        auto ps = util::string::split(tokens[1]);
        property_type = wordrep::WikidataUIDindex::get_uid(ps.front());
        std::swap(ps.back(), ps.front());
        ps.pop_back();
        for(auto p : ps)
            properties.push_back(wordrep::WikidataUIDindex::get_uid(p));
    }
    wordrep::WikidataUID entity;
    wordrep::WikidataUID property_type;
    std::vector<wordrep::WikidataUID> properties;
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
            if(item.property_type == p31) {
                p31_properties[item.entity] = item.properties;
                for (auto property : item.properties)
                    instance_of[property].push_back(item.entity);
            }
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
        if(x==instance_of.cend()) return {{}};
        return {x->second};
    }
    std::vector<wordrep::WikidataUID> get_instance_uids(wordrep::WikidataUID uid) const{
        auto x = instance_of.find(uid);
        if(x==instance_of.cend()) return {};
        return x->second;
    }
    std::vector<wordrep::WikidataUID> get_p31_properties(wordrep::WikidataUID uid) const{
        auto x = p31_properties.find(uid);
        if(x==p31_properties.cend()) return {};
        return x->second;
    }
    std::map<wordrep::WikidataUID,std::vector<wordrep::WikidataUID>> instance_of;
    std::unordered_map<wordrep::WikidataUID,std::vector<wordrep::WikidataUID>> p31_properties;
};

}//namespace wikidata
