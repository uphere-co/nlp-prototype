#include "similarity/config.h"

#include <ostream>

#include <fmt/printf.h>

#include "utils/hdf5.h"
#include "utils/versioned_name.h"

namespace engine{

wikidata::EntityModuleBuilder::Param get_wikimoudle_param(Config const &config){
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
    return {config.value("word_uids_dump")};
}
wordrep::POSUIDindex SubmoduleFactory::pos_uid_index() const {
    return {config.value("pos_uids_dump")};
}
wordrep::ArcLabelUIDindex SubmoduleFactory::arclabel_uid_index() const {
    return {config.value("arclabel_uids_dump")};
}
wordrep::DepParsedTokens SubmoduleFactory::dep_parsed_tokens() const {
    auto name = util::get_latest_version(config.value("dep_parsed_store"));
    if(data_minor_version){
        auto minor_version = data_minor_version.value();
        name = util::VersionedName{config.value("dep_parsed_store"), wordrep::DepParsedTokens::major_version, minor_version};
    }
    fmt::print(std::cerr, "Read {}\n", name .fullname);
    return {name , config.value("dep_parsed_prefix")};
}
wordrep::WordImportance SubmoduleFactory::word_importance() const {
    return {util::io::h5read(config.value("word_prob_dump"))};
}

wordrep::VocaInfo SubmoduleFactory::voca_info() const{
    return {config.value("wordvec_store"), config.value("voca_name"),
            config.value("w2vmodel_name"), config.value("w2v_float_t")};
}

wordrep::WordCaseCorrector SubmoduleFactory::word_case_corrector(wordrep::WordImportance const& importance) const {
    return {config.value("word_uids_dump"), importance};
}

Dataset SubmoduleFactory::empty_dataset() const{
    return {voca_info(),
            {config.value("word_uids_dump"),config.value("pos_uids_dump"),config.value("arclabel_uids_dump")}};
}
Dataset SubmoduleFactory::load_dataset() const{
    return {voca_info(),
            {config.value("word_uids_dump"),config.value("pos_uids_dump"),config.value("arclabel_uids_dump")},
            dep_parsed_tokens()};
}

data::DBIndexer SubmoduleFactory::db_indexer() const {
    return {util::io::h5read(util::get_latest_version(config.value("dep_parsed_store")).fullname),
            config.value("dep_parsed_prefix")};
};

wordrep::WikidataUIDindex SubmoduleFactory::wikientity_uid_index() const{
    return {config.value("wikidata_uids")};
}
wikidata::EntityModule SubmoduleFactory::wikientity_module() const{
    return wikidata::EntityModuleBuilder{get_wikimoudle_param(config)}.build();
}

}//namespace engine
