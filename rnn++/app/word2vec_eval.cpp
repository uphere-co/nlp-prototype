#include <fmt/printf.h>
#include <src/wordrep/dep_parsed.h>

#include "similarity/config.h"

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
    //using WordBlock = wordrep::WordBlock_base<UnigramDist::float_t,100>;
    using WordBlock = wordrep::WordBlock_base<VocaInfo::val_t,100>;
    ObjectiveScore(IndexedTexts const& texts,
                   WordBlock const& wvecs,
                   WordBlock const& cvecs,
                   word2vec::UnigramDist const& unigram,
                   util::Sampler<VocaIndex,UnigramDist::float_t> const& neg_sampler)
            : texts{texts}, wvecs{wvecs},cvecs{cvecs}, unigram{unigram}, neg_sampler{neg_sampler}
    {}

    float_t operator() (Chunk const& chunk, VocaInfo const& voca, WordUIDindex const& wordUIDs) const {
        auto seed = chunk.beg;
        std::mt19937 gen{seed};
        std::uniform_real_distribution<float_t> uni{0, neg_sampler.total_weight()};

        std::vector<VocaIndex> subsampled;
        subsampled.reserve(chunk.end - chunk.beg);
        for (auto i = chunk.beg; i != chunk.end; ++i) {
            //auto idx = texts.word(i);
            auto idx = voca.indexmap[texts.word_uid(i)];
            subsampled.push_back(idx);
        }
        auto n_neg = 5;
        auto len = util::singed_size(subsampled);
        float_t objective_sum=0.0;
        for (std::ptrdiff_t i = 0; i < len; ++i) {
            word2vec::WordContext context{i, subsampled, 5, 5};
            auto vidx = subsampled[context.self];
//            auto widx = voca.indexmap[vidx];
//            fmt::print("{:<15} {:<6}\n", wordUIDs[widx], vidx);
            auto w = wvecs[vidx];
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
    engine::SubmoduleFactory factory{{config}};
    util::MockTimer timer;

//    WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};
//    VocaInfo voca{config["wordvec_store"], config["voca_name"],
//                  config["w2vmodel_name"], config["w2v_float_t"]};
//    auto data_store = util::io::h5read(util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname);
//    IndexedTexts texts{data_store, config["dep_parsed_prefix"]};
//    word2vec::UnigramDist unigram{util::io::h5read("nyt_words.h5")};
//    util::Sampler<VocaIndex,UnigramDist::float_t> neg_sampler{unigram.get_neg_sample_dist(0.75)};

    std::string text_store  = util::get_latest_version(factory.config.value("dep_parsed_store")).fullname;
    std::string text_prefix = factory.config.value("dep_parsed_prefix");
    IndexedTexts texts{util::io::h5read(text_store), text_prefix};

    auto voca = factory.voca_info();
    auto wordUIDs = factory.word_uid_index();
    word2vec::UnigramDist unigram{util::io::h5read("unigram.h5"), voca.indexmap};
    util::Sampler<VocaIndex,UnigramDist::float_t> neg_sampler{unigram.get_neg_sample_dist(0.75)};

    //using WordBlock = wordrep::WordBlock_base<UnigramDist::float_t,100>;
    using WordBlock = wordrep::WordBlock_base<VocaInfo::val_t,100>;
    std::uniform_real_distribution<WordBlock::val_t> dist{-0.05,0.05};
    auto n_voca = unigram.size();
    fmt::print(std::cerr, "{} words in UnigramDist.\n", n_voca);
    auto& wvecs = voca.wvecs;
    auto& cvecs = voca.wvecs;
//    WordBlock wvecs{random_vector(voca.dim*voca.indexmap.size(),dist)};
//    WordBlock cvecs=wvecs;
//    WordBlock cvecs{random_vector(WordBlock::dim*n_voca,dist)};

    auto iter = util::IterChunkIndex_factory(texts.sents_uid.get());
    std::vector<std::pair<size_t,size_t>> chunks;
    while(auto maybe_chunk = iter.next()) chunks.push_back(maybe_chunk.value());
    auto n = chunks.size();

    double score_sum = 0.0;
    ObjectiveScore oscore_eval{texts,wvecs,cvecs, unigram,neg_sampler};
    auto n_sent=10000;
    for(int i=0; i<n_sent; ++i){
        auto& chunk = chunks[i];
        auto score = oscore_eval(chunk, voca, wordUIDs);
        score_sum += score;
//        fmt::print(std::cerr, "{}\n", score);
    }
    std::cerr<<fmt::format("{} : sum\n", score_sum/n_sent)<<std::endl;

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
