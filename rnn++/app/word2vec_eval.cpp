#include <fmt/printf.h>
#include <src/wordrep/dep_parsed.h>

#include "word2vec/word2vec.h"

#include "wordrep/wordvec.h"
#include "wordrep/word_uid.h"
#include "wordrep/word_iter.h"
#include "wordrep/word_count.h"
#include "wordrep/indexes.h"
#include "wordrep/indexed_text.h"
#include "wordrep/voca_info.h"

#include "utils/random.h"
#include "utils/persistent_vector.h"
#include "utils/string.h"
#include "utils/json.h"
#include "utils/profiling.h"
#include "utils/loop_gen.h"
#include "utils/versioned_name.h"

using wordrep::ChunkIndex;
using wordrep::SentUID;
using wordrep::WordUID;
using wordrep::VocaIndex;
using wordrep::IndexedTexts;
using word2vec::UnigramDist;

using wordrep::WordUIDindex;
using wordrep::VocaInfo;

struct Chunk{
    Chunk(std::pair<size_t,size_t> chunk)
            : beg{chunk.first}, end{chunk.second}
    {}
    size_t beg;
    size_t end;
};
struct ObjectiveScore{
    using float_t = UnigramDist::float_t;
    using WordBlock = wordrep::WordBlock_base<UnigramDist::float_t,100>;
    ObjectiveScore(IndexedTexts const& texts,
                   WordBlock const& wvecs,
                   WordBlock const& cvecs,
                   word2vec::UnigramDist const& unigram,
                   util::Sampler<VocaIndex,UnigramDist::float_t> const& neg_sampler)
            : texts{texts}, wvecs{wvecs},cvecs{cvecs}, unigram{unigram}, neg_sampler{neg_sampler}
    {}

    float_t operator() (Chunk const& chunk) const {
        auto seed = chunk.beg;
        std::mt19937 gen{seed};
        std::uniform_real_distribution<float_t> uni{0, neg_sampler.total_weight()};

        std::vector<VocaIndex> subsampled;
        subsampled.reserve(chunk.end - chunk.beg);
        for (auto i = chunk.beg; i != chunk.end; ++i) {
            auto idx = texts.word(i);
            subsampled.push_back(idx);
        }
        auto n_neg = 5;
        auto len = util::singed_size(subsampled);
        float_t objective_sum=0.0;
        for (std::ptrdiff_t i = 0; i < len; ++i) {
            word2vec::WordContext context{i, subsampled, 5, 5};
            auto w = wvecs[subsampled[context.self]];
            for (auto cword : context.contexts) {
                auto c = cvecs[subsampled[cword]];
                objective_sum += sigmoid(w,c);
                for (int j = 0; j != n_neg; ++j) {
                    auto cnidx = neg_sampler.sample(uni(gen));
                    auto cn = cvecs[cnidx];
                    objective_sum += sigmoid_plus(w,c);
                }
            }
        }
        return objective_sum;
    }

private:
    IndexedTexts const& texts;
    WordBlock const& wvecs;
    WordBlock const& cvecs;
    word2vec::UnigramDist const& unigram;
    util::Sampler<VocaIndex,UnigramDist::float_t> const& neg_sampler;
};
void evaluation(int argc, char** argv){
    //assert(argc>1);
    auto config = util::load_json(argv[1]);
    util::MockTimer timer;

//    WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};
//    VocaInfo voca{config["wordvec_store"], config["voca_name"],
//                  config["w2vmodel_name"], config["w2v_float_t"]};
//    auto data_store = util::io::h5read(util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname);
//    IndexedTexts texts{data_store, config["dep_parsed_prefix"]};
//    word2vec::UnigramDist unigram{util::io::h5read("nyt_words.h5")};
//    util::Sampler<VocaIndex,UnigramDist::float_t> neg_sampler{unigram.get_neg_sample_dist(0.75)};

    IndexedTexts texts{util::io::h5read("nyt_texts.h5"), "nyt"};

    word2vec::UnigramDist unigram{util::io::h5read("nyt_words.h5")};
    util::Sampler<VocaIndex,UnigramDist::float_t> neg_sampler{unigram.get_neg_sample_dist(0.75)};

    using WordBlock = wordrep::WordBlock_base<UnigramDist::float_t,100>;
    std::uniform_real_distribution<WordBlock::val_t> dist{-0.05,0.05};
    auto n_voca = unigram.size();
    WordBlock wvecs{random_vector(WordBlock::dim*n_voca,dist)};
    WordBlock cvecs{random_vector(WordBlock::dim*n_voca,dist)};

    auto iter = util::IterChunkIndex_factory(texts.sents_uid.get());
    std::vector<std::pair<size_t,size_t>> chunks;
    while(auto maybe_chunk = iter.next()) chunks.push_back(maybe_chunk.value());
    auto n = chunks.size();

    ObjectiveScore oscore_eval{texts,wvecs,cvecs, unigram,neg_sampler};
    auto& chunk = chunks[0];
    auto score = oscore_eval(chunk);
    std::cerr<<fmt::format("{}\n", score)<<std::endl;

//    std::random_device rd{};
//    auto seed = rd();
//    tbb::parallel_for(decltype(n){0}, n,
//                      [&timer, &texts, &chunks,&voca,&neg_sampler,seed]
//                              (auto &i_chunk) {
//                          auto chunk = chunks[i_chunk];
//
//                      });

}

int main(int argc, char** argv){
    evaluation(argc,argv);
    return 0;
}
