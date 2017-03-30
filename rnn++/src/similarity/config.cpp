#include "similarity/config.h"

#include <ostream>

#include <fmt/printf.h>

#include "utils/hdf5.h"
#include "utils/versioned_name.h"

namespace engine{

SubmoduleFactory::SubmoduleFactory(Config const& config)
        : config{config}
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
    auto latest_version = util::get_latest_version(config.value("dep_parsed_store"));
    fmt::print(std::cerr, "Read {}\n", latest_version.fullname);
    return {latest_version, config.value("dep_parsed_prefix")};
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
    return wikidata::EntityModuleBuilder{}.build(
            {config.value("word_uid_bin")},
            {config.value("pos_uid_bin")},
            {config.value("wikidata_entities_by_name")},
            {config.value("wikidata_entities_by_uid")},
            {config.value("wikidata_properties")},
            {config.value("wikidata_instances")},
            {config.value("named_entity_uids")},
            {config.value("wikidata_uids")});
}

}//namespace engine
