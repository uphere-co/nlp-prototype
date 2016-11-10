#include <unordered_map>
#include "wordrep/voca.h"

#include "fmt/printf.h"

#include "tests/test_sent2vec.h"

#include "models/sentence2vec.h"

#include "utils/profiling.h"
#include "utils/string.h"
#include "utils/json.h"

using namespace util;
using namespace util::io;
using namespace wordrep;

namespace sent2vec{
namespace test{

void word_count(util::json_t const &config){
    Timer timer{};
    //std::unordered_map<VocaIndex,size_t> wc_serial, wc;
    using wcounts_t = std::unordered_map<std::string,size_t>;
    wcounts_t wc_serial, owc_serial;
    wcounts_t wc, owc;
    wcounts_t pos_count, arclabel_count;
    auto files = string::readlines("results.1k");
    timer.here_then_reset("Begins serial word count");
    for(auto file : files){
        auto parsed_json = load_json(file);
        for(auto const& sent_json : parsed_json["sentences"] ) {
            for (auto const &token : sent_json["tokens"]) {
                auto originalText = token["originalText"].get<std::string>();
                auto word = token["word"].get<std::string>();
                auto pos = token["pos"].get<std::string>();
                ++wc[word];
                ++owc[originalText];
                ++pos_count[pos];
            }
            for (auto const &x : sent_json["basicDependencies"]) {
                auto arc_label = x["dep"].get<std::string>();
                ++arclabel_count[arc_label];
            }
        }
    }
    timer.here_then_reset("Finish serial word count.");
    //std::vector<wcounts_t::value_type> wc_sorted;
    std::vector<std::pair<wcounts_t::key_type,wcounts_t::mapped_type>> wc_sorted;
    //std::vector<std::pair<std::string,size_t>> wc_sorted;
    for(auto x : wc) wc_sorted.push_back(x);
    std::sort(wc_sorted.begin(), wc_sorted.end(), [](auto x, auto y){return x.second>y.second;});
    timer.here_then_reset("Sort word counts.");

    for(auto x : wc_sorted) fmt::print("{} {}\n", x.first, x.second);
    fmt::print("\n");
}

void sampler(){
    Timer timer{};
    H5file file{H5name{"wordvec.h5"}, hdf5::FileMode::read_exist};
    UnigramDist unigram{file, "1b.short_sents.bar.word_key", "1b.short_sents.word_count"};
    timer.here_then_reset("Voca loaded.");
    NegativeSampleDist neg_sample_dist{unigram.prob, 0.75};
    auto negative_sampler=neg_sample_dist.get_sampler();
    Sampler2 negative_sampler2{neg_sample_dist.dist, 1000000};
    val_t sum_dist{};
    for(auto x:neg_sample_dist.dist) sum_dist+=x;
    std::cout<<sum_dist<<std::endl;
    std::random_device rd;
    std::mt19937 gen{rd()};
    std::uniform_real_distribution<val_t> ur=negative_sampler2.get_pdf();
    timer.here_then_reset("Loop begins.");
    auto sum=0.0;
    auto n=1000000;
    for(int i=0; i<n; ++i){
        // auto widx = negative_sampler(gen);
        auto widx = negative_sampler2(ur(gen));
        sum += widx;
        // sum += ur(gen);
        // std::cout<<unigram.voca.getWord(widx).val<<std::endl;
    }
    std::cout<<sum/n<<std::endl;
    timer.here_then_reset("Loop ends.");
}

void negative_sampling(){
    H5file file{H5name{"data.h5"}, hdf5::FileMode::read_exist};
    UnigramDist word_dist{file, "1b.short_sents.bar.word_key", "1b.short_sents.word_count"};
    // Sampler<UnigramDist::iter_t> sampler{word_dist.prob.cbegin(),word_dist.prob.cend()};
    // NegativeSampleDist neg_sample_dist{word_dist.prob, 0.0};
    NegativeSampleDist neg_sample_dist{word_dist.prob, 0.75};
    auto sampler=neg_sample_dist.get_sampler();

    std::random_device rd;
    std::mt19937 gen{rd()};

    std::map<int, int> m;
    for(int n=0; n<100000; ++n) {
        ++m[sampler(gen)];
    }
    std::map<int, int, std::greater<int>> m_inv;
    for(auto const x:m) m_inv[x.second]=x.first;
    for(auto p : m_inv) {
        std::cout << p.second << " " <<word_dist.voca.getWord(p.second).val <<" generated " << p.first << " times\n";
    }
    std::cerr<<"Voca size: "<<word_dist.prob.size()<<std::endl;
}



auto print_word=[](auto widx, auto const &word_dist){
    print(word_dist.voca.getWord(widx).val);
};
auto print_context=[](auto const &context, auto const &word_dist){
    print(word_dist.voca.getWord(context.widx).val);
    print(": ");
    for(auto idx: context.cidxs){
        if(is_unknown_widx(idx))  {print("-UNKNOWN-"); continue;}
        print_word(idx, word_dist);
    }
    print('\n');
};


void context_words(){
    H5file file{H5name{"data.h5"}, hdf5::FileMode::read_exist};
    UnigramDist unigram{file, "1b.short_sents.bar.word_key", "1b.short_sents.word_count"};
    auto word2idx = unigram.voca.indexing();
    std::random_device rd;
    std::mt19937 gen{rd()};
    std::uniform_real_distribution<val_t> uni01{0.0,1.0};

    rnn::TokenizedSentences dataset{"testset"};
    auto& lines = dataset.val;
    SubSampler sub_sampler{0.00001, unigram};
    for(size_t sidx=0; sidx<lines.size(); ++sidx){
        auto& sent = lines[sidx];
        auto widxs_orig = word2idx.getIndex(sent);
        auto widxs = sub_sampler(widxs_orig, uni01(gen));
        for(auto widx:widxs) assert(!is_unknown_widx(widx));
        for(auto self=widxs.cbegin(); self!=widxs.end(); ++self){
            print_context(SentVecContext{sidx, self, widxs, 5,5}, unigram);
        }
    }

}

}//namespace sent2vec::test
}//namespace sent2vec
