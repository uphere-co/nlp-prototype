#include "wordrep/sentence2vec.h"

#include "parser/parser.h"
#include "parser/wordvec.h"
#include "utils/profiling.h"
#include "utils/parallel.h"
#include "utils/print.h"

using namespace rnn;
using namespace rnn::simple_model;
using namespace rnn::wordrep;
using namespace util::math;
using namespace util::io;
using namespace util;
using namespace sent2vec;


int main(){
    using val_t = double;
    using idx_t = std::size_t;
    constexpr int word_dim=100;
    constexpr util::DataType w2vmodel_f_type = util::DataType::dp;

    Timer timer{};
    // H5file file{H5name{"gensim.h5"}, hdf5::FileMode::read_exist};
    // UnigramDist unigram{file, "1b.training.1M.gensim.word", "1b.training.1M.gensim.count"};
    H5file file{H5name{"data.1M.h5"}, hdf5::FileMode::read_exist};
    UnigramDist unigram{file, "1b.training.1M.word", "1b.training.1M.count"};
    // H5file file{H5name{"data.1M.h5"}, hdf5::FileMode::read_exist};
    // UnigramDist unigram{file, "1b.training.1M.word", "1b.training.1M.count"};
    //Voca const &voca = unigram.voca;
    Voca voca = load_voca("data.1M.h5", "1b.training.1M.word");
    auto voca_vecs = load_voca_vecs<word_dim>("data.1M.h5", "1b.training.1M", w2vmodel_f_type);
    // Voca voca = load_voca("gensim.h5", "1b.training.1M.gensim.word");
    // auto voca_vecs = load_voca_vecs<word_dim>("gensim.h5", "1b.training.1M.gensim", w2vmodel_f_type);
    VocaIndexMap word2idx = voca.indexing();
    auto voca_size = voca.size();
    print("Voca size:");
    print(voca_size);
    print("\n");
    timer.here_then_reset("Data loaded.");

    
    SubSampler sub_sampler{0.0001, unigram};
    OccurrenceFilter freq_filter{5, unigram};
    auto filtered_words=[&freq_filter,&sub_sampler](auto const &widxs_orig, auto ran){
        auto widxs_filtered = freq_filter(widxs_orig);
        auto widxs = sub_sampler(widxs_filtered, ran);
        // auto widxs = sub_sampler(widxs_orig, ran);
        return widxs;
    };
    
    TokenizedSentences dataset{"1b.trainset.1M"};
    // TokenizedSentences dataset{"1b.trainset"};
    timer.here_then_reset("Train dataset loaded.");

    auto& lines = dataset.val;
    auto n=lines.size();
    std::vector<std::vector<idx_t>> sent_widxs(lines.size());
    tbb::parallel_for(decltype(n){0}, n, [&](decltype(n) i){    
        sent_widxs[i]=word2idx.getIndex(lines[i]);
    });
    auto scoring_sentence=[&](auto const &widxs_orig){
        std::random_device rd{};
        std::mt19937 gen{rd()};
        std::uniform_real_distribution<val_t> uni01{0.0,1.0};
        auto widxs = filtered_words(widxs_orig, uni01(gen));
        return scoring_words(widxs, voca_vecs);
    };
    auto scoring_dataset=[&](){return parallel_reducer(sent_widxs.cbegin(),sent_widxs.cend(),
                                                       scoring_sentence, val_t{0});};
    print(scoring_dataset());
    print("\n");
}