#include "wordrep/preprocessed_sentences.h"

#include "wordrep/serialized_annotation.h"

namespace wordrep{

PreprocessedSentences PreprocessedSentences::factory(std::vector<Sentence> const& sents,
                                                     AnnotationData const& annotation){
    PreprocessedSentences tagged_sents;
    tagged_sents.data->reserve(sents.size());
    for(auto& sent: sents) tagged_sents.data->push_back({sent});
    auto n_sents = sents.size();
    tbb::parallel_for(decltype(n_sents){0},n_sents, [&sents,&tagged_sents](auto i){
        auto& sent = sents[i];
        auto& tagged_sent = tagged_sents.data->at(i);
        assert(sent.uid == tagged_sent.orig.uid);
    });

    auto n_block = annotation.blocks.size();
    tbb::parallel_for(decltype(n_block){0}, n_block, [&annotation,&tagged_sents](auto i_block){
        auto& block = annotation.blocks[i_block];
        auto candi_per_entity = block->iter_ambu().begin();
        for(auto& entity : block->tagged_tokens){
            SentUID suid     = entity.sent_uid();
            DPTokenIndex idx = entity.token_idx();
            auto token_len   = entity.token_len();
            auto& sent = tagged_sents.data->at(suid.val);

            if(!token_len) {
                sent.words.push_back({sent.orig, idx});
                continue;
            }
            assert(candi_per_entity.index()==idx);

            ConsecutiveTokens words{idx,token_len};
            auto candidates = *candi_per_entity;
            wiki::AmbiguousUID uid;
            for(auto candi : candidates) uid.candidates.push_back(candi.uid);
            auto& dict    = *sent.orig.dict;
            auto word_gov = dict.head_uid(idx);
            auto gov      = dict.head_word(idx);
            wordrep::Scoring::AmbiguousEntity x{candidates,uid, words, word_gov,gov};
            sent.entities.push_back(x);
            ++candi_per_entity;
        }
    });

    return tagged_sents;
}

}//namespace wordrep
