#include "similarity/config.h"

#include <ostream>

#include <fmt/printf.h>

#include "utils/hdf5.h"
#include "utils/versioned_name.h"

namespace engine{

wikidata::EntityModule::InputParam get_wikimoudle_param(wordrep::ConfigParams const &conf){
    return {
            conf.word_uid,
            conf.pos_uid,
            conf.entity_names,
            conf.entity_uids,
            {conf.entity_properties.name},
            {conf.entity_instances.name},
            conf.named_entity_uid,
            conf.wiki_uid
            };
}

SubmoduleFactory::SubmoduleFactory(util::json_t const& config_, std::optional<int> data_minor_version)
        : data_minor_version{data_minor_version}, conf{config_}
{}

data::CoreNLPwebclient SubmoduleFactory::corenlp_webclient() const {
    return {conf.corenlp_client.name};
}
wordrep::WordUIDindex SubmoduleFactory::word_uid_index() const {
    return {conf.word_uid};
}
wordrep::POSUIDindex SubmoduleFactory::pos_uid_index() const {
//    return {config.value("pos_uids_dump")};
    return {conf.pos_uid};
}
wordrep::ArcLabelUIDindex SubmoduleFactory::arclabel_uid_index() const {
    return {conf.arclabel_uid.name};
}
wordrep::DepParsedTokens SubmoduleFactory::dep_parsed_tokens() const {
    //TODO:refactoring
     return wordrep::DepParsedTokens::factory(conf.parsed_text);
}
wordrep::WordImportance SubmoduleFactory::word_importance() const {
    //TODO:refactoring
    return {util::io::h5read(conf.word_imporance.name)};
}

wordrep::VocaInfo SubmoduleFactory::voca_info() const{
    namespace fb = util::io::fb;
    std::vector<wordrep::WordUID> vidx_wuids;
    std::vector<float> wvecs_raw;

    util::parallel_invoke(
            //TODO:refactoring
            [&vidx_wuids,this](){fb::deserialize_i64vector(fb::load_binary_file(this->conf.voca_idx.name), vidx_wuids);},
            [&wvecs_raw,this](){fb::deserialize_f32vector(fb::load_binary_file(this->conf.word_vecs.name), wvecs_raw);}
    );

    return {{vidx_wuids},{wvecs_raw}};
}

wordrep::WordCaseCorrector SubmoduleFactory::word_case_corrector(wordrep::WordImportance const& importance) const {
    //TODO:refactoring
     return {conf.word_list.name, importance};
}

wordrep::AnnotationData SubmoduleFactory::load_annotation() const{
    //TODO:refactoring
    return wordrep::AnnotationData::factory(conf.annotated_tokens);
}

Dataset SubmoduleFactory::empty_dataset() const{
    return {voca_info()};
}
Dataset SubmoduleFactory::load_dataset() const{
    return {voca_info(), dep_parsed_tokens()};
}

data::DBIndexer SubmoduleFactory::db_indexer() const {
    return {conf.parsed_text};
};

wordrep::WikidataUIDindex SubmoduleFactory::wikientity_uid_index() const{
    return {conf.wiki_uid};
}
wikidata::EntityModule SubmoduleFactory::wikientity_module() const{
    return wikidata::EntityModule::factory(get_wikimoudle_param(conf));
}

}//namespace engine
