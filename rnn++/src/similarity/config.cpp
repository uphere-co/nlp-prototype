#include "similarity/config.h"

#include <ostream>

#include <fmt/printf.h>

#include "utils/hdf5.h"
#include "utils/versioned_name.h"

namespace engine{

wikidata::EntityModule::InputParam get_wikimoudle_param(Config const &config){
    auto conf = [&config](auto x){return config.value(x);};
    return {
            wordrep::UIDIndexBinary{conf("word_uid_bin")},
            wordrep::UIDIndexBinary{conf("pos_uid_bin")},
            wordrep::wiki::SortedEntities::Binary{conf("wikidata_entities_by_name")},
            wordrep::wiki::UIDSortedEntities::Binary{conf("wikidata_entities_by_uid")},
            util::io::fb::PairsBinary{conf("wikidata_properties")},
            util::io::fb::PairsBinary{conf("wikidata_instances")},
            wordrep::UIDIndexBinary{conf("named_entity_uids")},
            wordrep::UIDIndexBinary{conf("wikidata_uids")}
    };
}

SubmoduleFactory::SubmoduleFactory(Config const& config, std::optional<int> data_minor_version)
        : config{config}, data_minor_version{data_minor_version}
{}

data::CoreNLPwebclient SubmoduleFactory::corenlp_webclient() const {
    return {config.value("corenlp_client_script")};
}
wordrep::WordUIDindex SubmoduleFactory::word_uid_index() const {
    return {wordrep::UIDIndexBinary{config.value("word_uid_bin")}};
    //return {config.value("word_uids_dump")};
}
wordrep::POSUIDindex SubmoduleFactory::pos_uid_index() const {
//    return {config.value("pos_uids_dump")};
    return {wordrep::UIDIndexBinary{config.value("pos_uid_bin")}};
}
wordrep::ArcLabelUIDindex SubmoduleFactory::arclabel_uid_index() const {
    return {config.value("arclabel_uids_dump")};
}
wordrep::DepParsedTokens SubmoduleFactory::dep_parsed_tokens() const {
    return wordrep::DepParsedTokens::factory({config.value("dep_parsed_bins")});
}
wordrep::WordImportance SubmoduleFactory::word_importance() const {
    return {util::io::h5read(config.value("word_prob_dump"))};
}

wordrep::VocaInfo SubmoduleFactory::voca_info() const{
    namespace fb = util::io::fb;
    std::vector<wordrep::WordUID> vidx_wuids;
    std::vector<float> wvecs_raw;

    auto conf = [this](auto x){return this->config.value(x);};
    util::parallel_invoke(
            [&vidx_wuids,&conf](){fb::deserialize_i64vector(fb::load_binary_file(conf("voca_bin")), vidx_wuids);},
            [&wvecs_raw,&conf](){fb::deserialize_f32vector(fb::load_binary_file(conf("w2vmodel_bin")), wvecs_raw);}
    );

    return {{vidx_wuids},{wvecs_raw}};
}

wordrep::WordCaseCorrector SubmoduleFactory::word_case_corrector(wordrep::WordImportance const& importance) const {
    return {config.value("words_list"), importance};
}

wordrep::AnnotationFile SubmoduleFactory::load_annotation() const{
    return wordrep::AnnotationFile::factory({config.value("annotated_tokens"),
                                             std::stoi(config.value("annotated_tokens_n_block"))});
}

Dataset SubmoduleFactory::empty_dataset() const{
    return {voca_info()};
}
Dataset SubmoduleFactory::load_dataset() const{
    return {voca_info(), dep_parsed_tokens()};
}

data::DBIndexer SubmoduleFactory::db_indexer() const {
    return {config.value("dep_parsed_bins")};
};

wordrep::WikidataUIDindex SubmoduleFactory::wikientity_uid_index() const{
    return {config.value("wikidata_uids")};
}
wikidata::EntityModule SubmoduleFactory::wikientity_module() const{
    return wikidata::EntityModule::factory(get_wikimoudle_param(config));
}

}//namespace engine
