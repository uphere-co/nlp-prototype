#include <fmt/printf.h>

#include "word2vec/word2vec.h"

#include "wordrep/word_uid.h"
#include "wordrep/word_iter.h"
#include "wordrep/word_count.h"
#include "wordrep/indexes.h"
#include "wordrep/wordvec.h"

#include "utils/persistent_vector.h"
#include "utils/profiling.h"
#include "utils/string.h"
#include "utils/json.h"

using wordrep::ChunkIndex;
using wordrep::SentUID;
using wordrep::WordUID;

void iter_sentences(int argc, char** argv){
    assert(argc>1);
    using util::io::h5read;
    auto file = h5read("texts.h5");
    std::string prefix =  "ygp";
    const util::TypedPersistentVector<ChunkIndex> chunks_idx{file,prefix+".chunk_idx"};
    const util::TypedPersistentVector<SentUID> sents_uid {file,prefix+".sent_uid"};
    const util::TypedPersistentVector<WordUID> words_uid {file,prefix+".word_uid"};

    util::Timer timer;
    auto config = util::load_json(argv[1]);
    auto words = util::string::readlines(util::get_str(config,"word_uids_dump"));
    timer.here_then_reset("Load wordUIDs");
    wordrep::TokenHash<wordrep::WordUID> hasher;
    std::map<WordUID,std::string> wuid2str;
    for(auto word : words) wuid2str[hasher(word)]=word;
    timer.here_then_reset("Build table.");

    using namespace word2vec;
    UnigramDist unigram{h5read("words.h5")};
    SubSampler subsampler{0.001, unigram};

    auto iter = util::IterChunkIndex_factory(sents_uid.get());
    while(auto maybe_chunk = iter.next()){
        auto chunk = maybe_chunk.value();
        fmt::print("{} {}\n", chunk.first, chunk.second);
        for(auto i=chunk.first; i!=chunk.second; ++i){
            fmt::print("{} ", wuid2str[words_uid[i]]);
        }
        fmt::print("\n");

        for(auto i=chunk.first; i!=chunk.second; ++i){
            auto uid = words_uid[i];
            if(!subsampler(unigram.voca[uid])) continue;
            fmt::print("{} ", wuid2str[uid]);
        }
        fmt::print("\n");

        for(auto i=chunk.first; i!=chunk.second; ++i){
            assert(i>=0);
            WordContext context{i, chunk.first, chunk.second, 5, 5};
            fmt::print("{} : ", wuid2str[words_uid[context.self]]);
            for(auto cword : context.contexts)
                fmt::print("{} ", wuid2str[words_uid[cword]]);
            fmt::print("\n");
        }
        fmt::print("\n");
    }
}

template<typename T>
std::vector<T> random_vec(size_t len){
    std::random_device rd{};
    auto seed = rd();
    std::vector<T> vec(len);
    auto n=vec.size();
    tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n),
                      [&vec,seed](tbb::blocked_range<decltype(n)> const &r){
                          std::uniform_real_distribution<T> uni{-0.05,0.05};
                          std::mt19937 gen{seed+r.begin()};
                          for(decltype(n) i=r.begin(); i!=r.end(); ++i)
                              vec[i]= uni(gen);
                      });
    return vec;
}

void training(int argc, char** argv){
    using WordBlock = wordrep::WordBlock_base<double,100>;
    WordBlock voca_vecs=wordrep::random_WordBlock<WordBlock::dim>(voca_size);
    auto adagrad_factor=init_WordBlock<WordBlock::dim>(voca_size, val_t{0.000001});
}
int main(int argc, char** argv) {
    iter_sentences(argc, argv);
    return 0;
}