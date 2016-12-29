#include <fmt/printf.h>

#include "word2vec/word2vec.h"

#include "wordrep/wordvec.h"
#include "wordrep/word_uid.h"
#include "wordrep/word_iter.h"
#include "wordrep/word_count.h"
#include "wordrep/indexes.h"

#include "utils/persistent_vector.h"
#include "utils/string.h"
#include "utils/json.h"
#include "utils/profiling.h"
#include "utils/loop_gen.h"

using wordrep::ChunkIndex;
using wordrep::SentUID;
using wordrep::WordUID;
using word2vec::UnigramDist;

struct IndexedTexts{
    IndexedTexts(util::io::H5file const &file, std::string prefix)
            : chunks_idx{file,prefix+".chunk_idx"},
              sents_uid {file,prefix+".sent_uid"},
              words_uid {file,prefix+".word_uid"},
              words {file,prefix+".word"}
    {}
    wordrep::WordUID   word_uid(size_t n) const {return words_uid[n];}
    wordrep::VocaIndex word(size_t n) const {return words[n];}

    util::TypedPersistentVector<wordrep::ChunkIndex> chunks_idx;
    util::TypedPersistentVector<wordrep::SentUID> sents_uid;
    util::TypedPersistentVector<wordrep::WordUID> words_uid;
    util::TypedPersistentVector<wordrep::VocaIndex> words;
};

void check_word_uid(int argc, char** argv){
    assert(argc>1);
    auto config = util::load_json(argv[1]);

    UnigramDist unigram{util::io::h5read("nyt_words.h5")};
    wordrep::WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};
    wordrep::VocaIndexMap voca{wordrep::load_voca(config["wordvec_store"], config["voca_name"])};

    auto words = util::string::readlines(util::get_str(config,"word_uids_dump"));
//    for(auto word : words){
//        fmt::print("{} {}\n", word, unigram.count)
//    }
}
void iter_sentences(int argc, char** argv){
    assert(argc>1);
    auto config = util::load_json(argv[1]);

    auto file = util::io::h5read("nyt_texts.h5");
    std::string prefix = "nyt";
    IndexedTexts texts{file, prefix};

    util::Timer timer;
    wordrep::VocaIndexMap voca{wordrep::load_voca(config["wordvec_store"], config["voca_name"])};
    auto words = util::string::readlines(util::get_str(config,"word_uids_dump"));
    timer.here_then_reset("Load wordUIDs");
    wordrep::TokenHash<wordrep::WordUID> hasher;
    std::map<WordUID,std::string> wuid2str;
    for(auto word : words) wuid2str[hasher(word)]=word;
    timer.here_then_reset("Build table.");

    using namespace word2vec;
//    UnigramDist unigram{util::io::h5read("words.h5")};
//    SubSampler subsampler{0.001, unigram};

    auto iter = util::IterChunkIndex_factory(texts.sents_uid.get());
    while(auto maybe_chunk = iter.next()){
        auto chunk = maybe_chunk.value();
        fmt::print("{} {}\n", chunk.first, chunk.second);
        for(auto i=chunk.first; i!=chunk.second; ++i){
            fmt::print("{} ", wuid2str[voca[texts.word(i)]]);
        }
        fmt::print("\n");

//        for(auto i=chunk.first; i!=chunk.second; ++i){
//            auto uid = texts.word(i);
//            if(!subsampler(unigram.voca[uid])) continue;
//            fmt::print("{} ", wuid2str[uid]);
//        }
//        fmt::print("\n");
//
//        for(auto i=chunk.first; i!=chunk.second; ++i){
//            assert(i>=0);
//            WordContext context{i, chunk.first, chunk.second, 5, 5};
//            fmt::print("{} : ", wuid2str[texts.word(context.self)]);
//            for(auto cword : context.contexts)
//                fmt::print("{} ", wuid2str[texts.word(cword)]);
//            fmt::print("\n");
//        }
//        fmt::print("\n");
    }
}

//T is RandomNumberDistribution
template<typename T>
std::vector<typename T::result_type> random_vector(size_t len, T dist){
    std::random_device rd{};
    auto seed = rd();
    std::vector<typename T::result_type> vec(len);
    auto n=vec.size();
    tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n),
                      [dist,seed,&vec] (tbb::blocked_range<decltype(n)> const &r)  {
                          std::mt19937 gen{seed+r.begin()};
                          auto d=dist;
                          for(auto i=r.begin(); i!=r.end(); ++i)
                              vec[i]= d(gen);
                      });
    return vec;
}
template<typename T>
std::vector<typename T::result_type> random_vector_serial(size_t len, T dist){
    std::random_device rd{};
    auto seed = rd();
    std::vector<typename T::result_type> vec(len);
    std::mt19937 gen{seed};
    for(auto& x : vec) x = dist(gen);
    return vec;
}
void training(int argc, char** argv){
    using WordBlock = wordrep::WordBlock_base<double,100>;
    std::uniform_real_distribution<WordBlock::val_t> dist{-0.05,0.05};
    util::Timer timer;
    auto n_voca = 100000;
    auto vec_init = random_vector(WordBlock::dim*n_voca,dist);
    timer.here_then_reset("Created random vector.");
    {
        auto vec_init = random_vector_serial(WordBlock::dim * n_voca, dist);
        timer.here_then_reset("Created random vector using a single thread version.");
    }
    fmt::printf("{}\n", util::math::sum(vec_init));
    timer.here_then_reset("Check summation.");
}

int main(int argc, char** argv){
    iter_sentences(argc,argv);
    training(argc,argv);
    return 0;
}