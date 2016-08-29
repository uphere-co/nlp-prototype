#include "wordrep/sentence2vec.h"
#include "utils/print.h"
#include "utils/profiling.h"

/*
-UNKNOWN-
-LPB- (
-RPB- )
*/
namespace sent2vec  {


}//namespace sent2vec




using namespace rnn::simple_model;
using namespace rnn::wordrep;
using namespace util::math;
using namespace util::io;
using namespace util;
using namespace sent2vec;
void test_negative_sampling(){
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

void test_context_words(){
    H5file file{H5name{"data.h5"}, hdf5::FileMode::read_exist};
    UnigramDist unigram{file, "1b.short_sents.bar.word_key", "1b.short_sents.word_count"};    
    VocaIndexMap word2idx = unigram.voca.indexing();
    std::random_device rd;
    std::mt19937 gen{rd()};
    std::uniform_real_distribution<val_t> uni01{0.0,1.0};
    
    TokenizedSentences dataset{"testset"};
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



void test_voca_update(){
    constexpr int word_dim=100;
    auto voca_size = 100;

    using WordBlock = WordBlock_base<word_dim>;
    WordBlock voca_vecs=random_WordBlock<word_dim>(voca_size);
    auto vec = voca_vecs[0];
    auto vec2 = voca_vecs[1];
    
    print(sum(voca_vecs[0]));
    for(auto &x:vec)x=1.0;
    // for(int i=0; i<100; ++i)voca_vecs[0][i]=2.5;
    print(sum(voca_vecs[0]));
    for(int i=0; i<100; ++i) vec[i]+=2.5; //vec==3.5
    print(sum(voca_vecs[0]));
    print(":sum\n");
    print(dot(vec, vec2));
    for(auto &x:vec2)x=2.0;
    print(dot(vec, vec2));
    voca_vecs.push_back(vec);//TODO: this cause undefined behaviour. Do not do push_back!!
    span_1d<val_t,word_dim> vec3 = voca_vecs[100]; //vec3==1
    // vec+=vec2; //vec==3
    // return;
    for(auto &x:vec)x=2.5;
    // assert(voca_vecs[0]==vec);
    // voca_vecs[0]+=voca_vecs[1];
    print(dot(vec, vec2));
    print(dot(voca_vecs[0], vec2));
    print(dot(vec3, vec2));
    print(":dot\n");
    return;
}




void test_word2vec_grad_update(){
    Timer timer{};
    // constexpr util::DataType w2vmodel_f_type = util::DataType::sp;
    constexpr int word_dim=100;

    H5file file{H5name{"data.h5"}, hdf5::FileMode::read_exist};
    UnigramDist unigram{file, "1b.short_sents.bar.word_key", "1b.short_sents.word_count"};
    NegativeSampleDist neg_sample_dist{unigram.prob, 0.75};    
    auto negative_sampler=neg_sample_dist.get_sampler();
    SubSampler sub_sampler{0.0001, unigram};
    OccurrenceFilter freq_filter{5, unigram};

    VocavecsGradientDescent optimizer{0.025};

    VocaIndexMap word2idx = unigram.voca.indexing();
    auto voca_size = unigram.voca.size();

    timer.here_then_reset("UnigramDist constructed");
    WordBlock voca_vecs=random_WordBlock<word_dim>(voca_size);
    std::cerr << "Sum: "<<sum(voca_vecs[10]) << std::endl;
    timer.here_then_reset("Initial WordBlock constructed");

    print(voca_size);
    print(": voca_size\n");
    timer.here_then_reset("Training begins");
    TokenizedSentences dataset{"testset"};
    auto& lines = dataset.val;    
    // for(auto const &sent:lines){
    auto n=lines.size();
    tbb::parallel_for(decltype(n){0}, n, [&](decltype(n) i){
        std::random_device rd;
        std::mt19937 gen{rd()};
        std::uniform_real_distribution<val_t> uni01{0.0,1.0};
        auto &sent=lines[i];
        auto widxs_orig = word2idx.getIndex(sent);
        auto widxs = sub_sampler(widxs_orig, uni01(gen));
        for(auto self=widxs.cbegin(); self!=widxs.end(); ++self){
            auto widx = *self;
            WordVecContext c_words{self, widxs, 5,5};
            for(auto cidx: c_words.cidxs) {
                auto cnidx = negative_sampler(gen);
                auto grad = vocavecs_gradient(voca_vecs, widx, cidx, cnidx);
                optimizer(voca_vecs, grad);
                print_word(widx, unigram);
                print_word(cidx, unigram);
                print_word(cnidx,unigram);
                print("\n");
            }
        }
    });
    //}
    timer.here_then_reset("test_word2vec_grad_update() is finished.");
    H5file h5store{H5name{"trained.h5"}, hdf5::FileMode::rw_exist};
    h5store.overwriteRawData(H5name{"testset"}, voca_vecs._val );
    timer.here_then_reset("Wrote word2vecs to disk.");
}

void test_sampler(){
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



int main(){
    // test_negative_sampling();
    // test_context_words();
    // test_voca_update();
    // test_word2vec_grad_update();
    // test_sampler();
    // return 0;

    Timer timer{};
    constexpr int word_dim=100;
    val_t alpha=0.025;

    H5file file{H5name{"data.1M.h5"}, hdf5::FileMode::rw_exist};
    UnigramDist unigram{file, "1b.training.1M.word", "1b.training.1M.count"};
    timer.here_then_reset("Voca loaded.");
    NegativeSampleDist neg_sample_dist{unigram.prob, 0.75};    
    auto negative_sampler=neg_sample_dist.get_sampler();
    const Sampler2 negative_sampler2{neg_sample_dist.dist, 100};
    SubSampler sub_sampler{0.001, unigram};
    VocaIndexMap word2idx = unigram.voca.indexing();
    timer.here_then_reset("Voca indexed.");
    auto voca_size = unigram.voca.size();
    WordBlock voca_vecs=random_WordBlock<word_dim>(voca_size);
    timer.here_then_reset("Initial WordBlock constructed");
    
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
        auto widxs = sub_sampler(widxs_orig, uni01(gen));
        return scoring_words(widxs, voca_vecs);
    };
    auto scoring_dataset=[&](){return parallel_reducer(sent_widxs.cbegin(),sent_widxs.cend(),
                                                       scoring_sentence, val_t{0});};
    timer.here_then_reset("Initial score");
    timer.here_then_reset("Training begins");
    for(int epoch=0; epoch<25; ++epoch){
        print("Epoch");
        print(epoch);
        print("Score: ");
        print(scoring_dataset());
        print("\n");

        std::random_device rd;
        auto seed = rd();

        // std::mt19937 gen{seed};
        // for(auto const &sent:lines){
        tbb::parallel_for(decltype(n){0}, n, [&](decltype(n) i){
            VecLoop_void<val_t,word_dim> vecloop_void{};
            auto ur=negative_sampler2.get_pdf();

            auto &widxs_orig=sent_widxs[i];            
            std::mt19937 gen{seed+i};
            std::uniform_real_distribution<val_t> uni01{0.0,1.0};
            auto widxs = sub_sampler(widxs_orig, uni01(gen));
            for(auto self=widxs.cbegin(); self!=widxs.end(); ++self){
                auto widx = *self;
                WordVecContext c_words{self, widxs, 5,5};
                auto w=voca_vecs[widx];
                for(auto cidx: c_words.cidxs) {
                    auto c=voca_vecs[cidx];
                    auto x_wc = alpha*(1-sigmoid(w,c));
                    vecloop_void(symm_fma_vec, x_wc, w, c);
                    // vecloop_void(fma_vec, c, x_wc, w);
                    // vecloop_void(fma_vec, w, x_wc, c);
                    for(int j=0; j<1; ++j){
                        // auto cnidx = ((int)ur(gen)+i)%voca_size;
                        auto cnidx=negative_sampler2(ur(gen));
                        // auto cnidx = negative_sampler(gen);
                        auto cn=voca_vecs[cnidx];
                        //grad_w : (1-sigmoid(w,c)) *c + (sigmoid_plus(w,c)-1) * c_n
                        //grad_c : (1-sigmoid(w,c)) * w 
                        //grad_cn : (sigmoid_plus(w,c)-1) *w
                        auto x_wcn = alpha*(sigmoid_plus(w,cn)-1);
                        vecloop_void(symm_fma_vec, x_wcn, w, cn);
                        // vecloop_void(fma_vec, w, x_wcn, cn);
                        // vecloop_void(fma_vec, cn, x_wcn, w);
                        // print_word(cnidx, unigram);
                    }
                }
            }
        });
        alpha *= 0.95;
        // alpha *= 0.8;
    }
    timer.here_then_reset("test_word2vec_grad_update() is finished.");
    //H5file h5store{H5name{"trained.h5"}, hdf5::FileMode::rw_exist};
    file.overwriteRawData(H5name{"1b.training.1M"}, voca_vecs._val );
    timer.here_then_reset("Wrote word2vecs to disk.");

    return 0;
}

