#include <fmt/printf.h>

#include "word2vec/word2vec.h"

#include "wordrep/wordvec.h"
#include "wordrep/word_uid.h"
#include "wordrep/word_iter.h"
#include "wordrep/word_count.h"
#include "wordrep/indexes.h"
#include "wordrep/indexed_text.h"

#include "utils/random.h"
#include "utils/string.h"
#include "utils/json.h"
#include "utils/profiling.h"
#include "utils/loop_gen.h"

using wordrep::ChunkIndex;
using wordrep::SentUID;
using wordrep::WordUID;
using wordrep::VocaIndex;
using wordrep::IndexedTexts;
using word2vec::UnigramDist;



void check_word_uid(int argc, char** argv){
    assert(argc>1);
    auto config = util::load_json(argv[1]);

    auto wordvec_file=util::io::h5read("nyt_words.h5");
    UnigramDist unigram{wordvec_file};
    wordrep::WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};
    wordrep::VocaIndexMap voca{wordrep::load_voca(config["wordvec_store"], config["voca_name"])};

    auto words = util::string::readlines(util::get_str(config,"word_uids_dump"));
//    for(auto word : words){
//        fmt::print("{} {}\n", word, unigram.count)
//    }
}
void iter_sentences(int argc, char** argv){
    //assert(argc>1);
    //auto config = util::load_json(argv[1]);

    auto file = util::io::h5read("nyt_texts.h5");
    std::string prefix = "nyt";
    IndexedTexts texts{file, prefix};

    util::Timer timer;
    auto wordvec_file=util::io::h5read("nyt_words.h5");
    util::TypedPersistentVector<WordUID> widx2wuid {wordvec_file, "widx2wuid"};
    wordrep::VocaIndexMap voca{widx2wuid.get()};

    auto words = util::string::readlines("/home/jihuni/word2vec/news/nyt.model.words");
    timer.here_then_reset("Load wordUIDs");
    wordrep::TokenHash<wordrep::WordUID> hasher;
    std::map<WordUID,std::string> wuid2str;
    for(auto word : words) wuid2str[hasher(word)]=word;
    timer.here_then_reset("Build table.");

    using namespace word2vec;
    std::random_device rd{};
    std::mt19937 gen{rd()};
    UnigramDist unigram{util::io::h5read("nyt_words.h5")};
    SubSampler subsampler{0.001, unigram};
    util::Sampler<VocaIndex,UnigramDist::float_t> neg_sampler{unigram.get_neg_sample_dist(0.75)};
    std::uniform_real_distribution<double> uni{0, neg_sampler.total_weight()};
    std::uniform_real_distribution<double> uni01{0.0,1.0};

    auto iter = util::IterChunkIndex_factory(texts.sents_uid.get());
    while(auto maybe_chunk = iter.next()){
        auto chunk = maybe_chunk.value();
        fmt::print("{} {}\n", chunk.first, chunk.second);
        for(auto i=chunk.first; i!=chunk.second; ++i){
            fmt::print("{} ", wuid2str[voca[texts.word(i)]]);
        }
        fmt::print("\n");

        std::vector<VocaIndex> subsampled;
        subsampled.reserve(chunk.second-chunk.first);
        for(auto i=chunk.first; i!=chunk.second; ++i){
            auto idx = texts.word(i);
            auto uid = texts.word_uid(i);
            //TODO: move this to unittest
            assert(voca[idx]==WordUID{-1}||voca[idx]==WordUID{hasher("-UNKNOWN-")}||voca[idx]==uid);
            if(!subsampler(idx,uni01(gen))) continue;
            subsampled.push_back(idx);
            fmt::print("{}({}) ", wuid2str[voca[idx]],unigram.get_prob(idx));
        }
        fmt::print("\n");

        auto len=util::singed_size(subsampled);
        for(std::ptrdiff_t i=0; i<len; ++i){
            WordContext context{i, subsampled, 5, 5};
            fmt::print("{} : ", wuid2str[voca[subsampled[context.self]]]);
            for(auto cword : context.contexts)
                fmt::print("{} ", wuid2str[voca[subsampled[cword]]]);
            fmt::print(" | ");
            for(int j=0; j!=5; ++j) fmt::print("{} ", wuid2str[voca[neg_sampler.sample(uni(gen))]]);
            fmt::print("\n");
        }
        fmt::print("\n");
    }
}


void training(int argc, char** argv){
    //assert(argc>1);
    //auto config = util::load_json(argv[1]);
    util::MockTimer timer;

    IndexedTexts texts{util::io::h5read("nyt_texts.h5"), "nyt"};

    word2vec::UnigramDist unigram{util::io::h5read("nyt_words.h5")};
    util::Sampler<VocaIndex,UnigramDist::float_t> neg_sampler{unigram.get_neg_sample_dist(0.75)};
    word2vec::SubSampler subsampler{0.001, unigram};

    //Setup word vector blocks
    using WordBlock = wordrep::WordBlock_base<UnigramDist::float_t,100>;
    WordBlock::val_t alpha=0.025;
    std::uniform_real_distribution<WordBlock::val_t> dist{-0.05,0.05};
    auto n_voca = unigram.size();
    WordBlock wvecs{random_vector(WordBlock::dim*n_voca,dist)};
    WordBlock cvecs{random_vector(WordBlock::dim*n_voca,dist)};

    auto iter = util::IterChunkIndex_factory(texts.sents_uid.get());

    std::random_device rd{};
    auto seed = rd();

    std::vector<std::pair<size_t,size_t>> chunks;
    while(auto maybe_chunk = iter.next()) chunks.push_back(maybe_chunk.value());
    auto n = chunks.size();
//    tbb::parallel_for(tbb::blocked_range<decltype(n)>{0,n,200},
//                      [&texts,&chunks,&unigram,&subsampler,&neg_sampler,&wvecs,&cvecs,alpha,seed](auto& r){
//    std::mt19937 gen{seed+util::to_signed_positive<decltype(seed)>(r.begin())};
    for(int epoch=0; epoch!=5; ++epoch) {
        tbb::parallel_for(decltype(n){0}, n,
                          [&timer, &texts, &chunks, &subsampler, &neg_sampler, &wvecs, &cvecs, alpha, seed]
                                  (auto &i_chunk) {
//    for(decltype(n)i_chunk=0; i_chunk!=n; ++i_chunk) {
                              timer.here_then_reset("loop begin");
                              std::mt19937 gen{seed + util::to_signed_positive<decltype(seed)>(i_chunk)};
                              std::uniform_real_distribution<double> uni{0, neg_sampler.total_weight()};
                              std::uniform_real_distribution<double> uni01{0.0, 1.0};
                              util::math::VecLoop_void<WordBlock::val_t, WordBlock::dim> vecloop_void{};
                              timer.here_then_reset("create objects");
//        for(auto i_chunk=r.begin(); i_chunk!=r.end(); ++i_chunk){
                              auto chunk = chunks[i_chunk];
                              std::vector<VocaIndex> subsampled;
                              subsampled.reserve(chunk.second - chunk.first);
                              for (auto i = chunk.first; i != chunk.second; ++i) {
                                  auto idx = texts.word(i);
                                  if (subsampler(idx, uni01(gen)))
                                      subsampled.push_back(idx);
                              }
                              timer.here_then_reset("sub_sampling");
                              auto len = util::singed_size(subsampled);
                              std::vector<VocaIndex> cns;
                              auto n_neg = 5;
                              for (int j = 0; j != 10 * n_neg * len; ++j) cns.push_back(neg_sampler.sample(uni(gen)));
                              int jj = 0;
                              timer.here_then_reset("neg_sampling");//about 50% of time is for 'uni(gen)' eval.
                              for (std::ptrdiff_t i = 0; i < len; ++i) {
                                  word2vec::WordContext context{i, subsampled, 5, 5};
                                  auto w = wvecs[subsampled[context.self]];
                                  for (auto cword : context.contexts) {
                                      auto c = cvecs[subsampled[cword]];
                                      auto x_wc = 1 - sigmoid(w, c);
                                      //plain gradient descent:
                                      vecloop_void(symm_fma_vec, alpha * x_wc, w, c);
                                      for (int j = 0; j != n_neg; ++j) {
                                          //TODO: fix thread safety
                                          auto cn = cvecs[cns[jj++]];
                                          //grad_w : (1-sigmoid(w,c)) *c + (sigmoid_plus(w,c)-1) * c_n
                                          //grad_c : (1-sigmoid(w,c)) * w
                                          //grad_cn : (sigmoid_plus(w,c)-1) *w
                                          auto x_wcn = sigmoid_plus(w, cn) - 1;
                                          vecloop_void(symm_fma_vec, alpha * x_wcn, w, cn);
                                      }
                                  }
                              }
                              timer.here_then_reset("vec updates");
                          });
    }

    auto len = n_voca*WordBlock::dim;
    using val_t = WordBlock::val_t;
    std::vector<val_t> word_vectors(len);
    auto tmp1 = wvecs.serialize();
    auto tmp2 = cvecs.serialize();
    for(decltype(len)i=0; i!=len; ++i) word_vectors[i]=0.5*(tmp1[i]+tmp2[i]);
    auto h5store=util::io::h5replace("nyt_vecs.h5");
    util::PersistentVector<val_t,val_t> w2v{"words.vecs", std::move(word_vectors)};
    w2v.write(h5store);
}

void test_random_vector_gen(){
    std::uniform_real_distribution<double> dist{-0.05,0.05};
    auto len = 1000000;
    util::Timer timer;
    auto vec_init = random_vector(len,dist);
    timer.here_then_reset(fmt::format("Created random vectors of {} words.", len));
    auto vec_init_serial = random_vector_serial(len,dist);
    timer.here_then_reset("Created random vector using a single thread version.");
    fmt::printf("{} vs {}\n", util::math::sum(vec_init), util::math::sum(vec_init_serial));
    timer.here_then_reset("Check summation.");
}

int main(int argc, char** argv){
//    iter_sentences(argc,argv);
    training(argc,argv);
    return 0;
}

