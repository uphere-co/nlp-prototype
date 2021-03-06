#include "similarity/dataset.h"

#include "utils/versioned_name.h"

using wordrep::SentUID;
using util::get_str;

namespace engine {

Dataset::Dataset(wordrep::VocaInfo&& voca)
        : voca{std::move(voca)}
{}
Dataset::Dataset(wordrep::VocaInfo&& voca,
                 wordrep::DepParsedTokens&& tokens_)
        : voca{std::move(voca)},
          tokens{std::move(tokens_)},
          sents{tokens.IndexSentences()},
          uid2sent{sents}
{}
Dataset::Dataset(Dataset&& data)
        : voca{std::move(data.voca)},
          tokens{std::move(data.tokens)},
          sents{tokens.IndexSentences()},
          uid2sent{sents},
          query_tokens_update{}
{}
Dataset::Dataset(Dataset const& data)
        : voca{data.voca},
          tokens{data.tokens},
          sents{tokens.IndexSentences()},
          uid2sent{sents},
          query_tokens_update{}
{}

std::vector<SentUID> Dataset::append_chunk(data::CoreNLPjson const &ask) {
    std::lock_guard<std::mutex> append_query_toekns{query_tokens_update};
    tokens.append_corenlp_output(ask);
    tokens.build_voca_index(voca.indexmap);
    auto uids = tokens.build_sent_uid(SentUID{SentUID::val_t{0x80000000}});
    sents = tokens.IndexSentences();
    uid2sent.add(sents);

    return uids;
}

}//namespace engine