#include "similarity/dataset.h"

#include "utils/versioned_name.h"

using wordrep::SentUID;
using util::get_str;

namespace engine {

UIDmaps::UIDmaps(std::string word_uids,
                 std::string pos_uids,
                 std::string arclabel_uids)
        : word{word_uids}, pos{pos_uids}, arclabel{arclabel_uids}
{}

Dataset::Dataset(wordrep::VocaInfo&& voca, UIDmaps &&token2uid)
        : voca{std::move(voca)}, token2uid{std::move(token2uid)}
{}
Dataset::Dataset(wordrep::VocaInfo&& voca, UIDmaps &&token2uid,
                 wordrep::DepParsedTokens&& tokens)
        : voca{std::move(voca)},
          token2uid{std::move(token2uid)},
          tokens{std::move(tokens)},
          sents{tokens.IndexSentences()},
          uid2sent{sents}
{}
Dataset::Dataset(Dataset&& data)
        : voca{std::move(data.voca)},
          token2uid{std::move(data.token2uid)},
          tokens{std::move(data.tokens)},
          sents{tokens.IndexSentences()},
          uid2sent{sents},
          query_tokens_update{}
{}
Dataset::Dataset(Dataset const& data)
        : voca{data.voca},
          token2uid{data.token2uid},
          tokens{data.tokens},
          sents{tokens.IndexSentences()},
          uid2sent{sents},
          query_tokens_update{}
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