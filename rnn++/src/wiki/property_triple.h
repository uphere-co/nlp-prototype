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
#include "utils/parallel_algorithm.h"

namespace wikidata{

struct PropertiesTriple{
    PropertiesTriple(std::string line){
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

struct EntityProperty{
    wordrep::WikidataUID entity;
    wordrep::WikidataUID property;
    friend bool operator==(EntityProperty x, EntityProperty y){ return x.entity==y.entity; }
    friend bool operator< (EntityProperty x, EntityProperty y){ return x.entity<y.entity; }
};
struct PropertyEntity{
    wordrep::WikidataUID property;
    wordrep::WikidataUID entity;
    friend bool operator==(PropertyEntity x, PropertyEntity y){ return x.property==y.property; }
    friend bool operator< (PropertyEntity x, PropertyEntity y){ return x.property<y.property; }
};

struct PropertyTable{
    PropertyTable(std::string file){
        std::ifstream is{file};
        tbb::task_group g;
        while (auto buffer=util::string::read_chunk(is, 2000000)) {
            auto& chars =  buffer.value();
            g.run([this, chars{std::move(chars)}](){
                std::stringstream ss;
                ss.str(chars.data());
                auto lines = util::string::readlines(std::move(ss));
                for(auto& line : lines){
                    wikidata::PropertiesTriple ps{line};
                    auto p31 = wordrep::WikidataUIDindex::get_uid("P31");
                    if(ps.property_type != p31) continue;
                    for(auto p : ps.properties){
                        this->p31_properties.push_back({ps.entity, p});
                        this->p31_instances.push_back({p, ps.entity});
                    }
                }
            });
        }
        g.wait();
        tbb::parallel_sort(p31_properties.begin(),p31_properties.end());
        tbb::parallel_sort(p31_instances.begin(), p31_instances.end());
    }
    PropertyTable(tbb::concurrent_vector<EntityProperty>&& p31_properties,
                  tbb::concurrent_vector<PropertyEntity>&& p31_instances)
            : p31_properties(std::move(p31_properties)),
              p31_instances(std::move(p31_instances))
    {}
    struct OpInstanceOf{
        bool is_instance(wordrep::WikidataUID uid) const {
            return util::isin(instances, uid);
        }
        std::vector<wordrep::WikidataUID> instances;
    };

    std::vector<wordrep::WikidataUID> get_instance_uids(wordrep::WikidataUID property) const{
        auto m_ps = util::binary_find_block(p31_instances, {property,-1});
        if(!m_ps) return {};
        std::vector<wordrep::WikidataUID> uids;
        auto ps = m_ps.value();
        for(auto it=ps.first; it!=ps.second; ++it) uids.push_back(it->entity);
        return uids;
    }
    std::vector<wordrep::WikidataUID> get_p31_properties(wordrep::WikidataUID entity) const{
        auto m_ps = util::binary_find_block(p31_properties, {entity,-1});
        if(!m_ps) return {};
        std::vector<wordrep::WikidataUID> uids;
        auto ps = m_ps.value();
        for(auto it=ps.first; it!=ps.second; ++it) uids.push_back(it->property);
        return uids;
    }
    OpInstanceOf get_op_instance_of(wordrep::WikidataUID property) const{
        return {get_instance_uids(property)};
    }
    tbb::concurrent_vector<EntityProperty> p31_properties; //P31 properties of entities
    tbb::concurrent_vector<PropertyEntity> p31_instances; // instances of P31 properties
};

}//namespace wikidata
