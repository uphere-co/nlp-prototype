#include "similarity/dataset.h"

#include "utils/versioned_name.h"

using wordrep::SentUID;

namespace engine {

UIDmaps::UIDmaps(util::json_t const& config)
        : word{config["word_uids_dump"].get<std::string>()},
          pos{config["pos_uids_dump"].get<std::string>()},
          arclabel{config["arclabel_uids_dump"].get<std::string>()}
{}

Dataset::Dataset(wordrep::VocaInfo&& voca, UIDmaps &&token2uid)
        : voca{std::move(voca)}, token2uid{std::move(token2uid)}
{}
Dataset::Dataset(json_t const &config)
        : voca{config["wordvec_store"], config["voca_name"],
               config["w2vmodel_name"], config["w2v_float_t"]},
          token2uid{config},
          tokens{util::get_latest_version(util::get_str(config, "dep_parsed_store")), config["dep_parsed_prefix"]},
          sents{tokens.IndexSentences()},
          uid2sent{sents}
{}

std::vector<SentUID> Dataset::append_chunk(data::CoreNLPjson const &ask) {
    std::lock_guard<std::mutex> append_query_toekns{query_tokens_update};
    tokens.append_corenlp_output(token2uid.word, token2uid.pos, token2uid.arclabel, ask);
    tokens.build_voca_index(voca.indexmap);
    auto uids = tokens.build_sent_uid(SentUID{SentUID::val_t{0x80000000}});
    sents = tokens.IndexSentences();
    uid2sent.add(sents);

    return uids;
}

}//namespace engine