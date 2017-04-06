#pragma once

#include <vector>
#include <mutex>

#include "wordrep/dep_parsed.h"
#include "wordrep/voca_info.h"
#include "wordrep/word_uid.h"

#include "utils/json.h"

namespace engine{

struct UIDmaps{
    wordrep::WordUIDindex word;
    wordrep::POSUIDindex pos;
    wordrep::ArcLabelUIDindex arclabel;
};

struct Dataset{
    using Sentence = wordrep::Sentence;
    using json_t = util::json_t;

    Dataset(wordrep::VocaInfo&& voca);
    Dataset(wordrep::VocaInfo&& voca,
            wordrep::DepParsedTokens&& tokens_);
    Dataset(Dataset&& data);
    Dataset(Dataset const& data);

    std::vector<wordrep::SentUID> append_chunk(data::CoreNLPjson const &ask);

    //TODO: clean up dependency on wordrep.
    wordrep::VocaInfo voca;

    wordrep::DepParsedTokens tokens{};
    std::vector<Sentence> sents{};
    wordrep::Sentences uid2sent{};
    std::mutex query_tokens_update{};
};

}//namespace engine