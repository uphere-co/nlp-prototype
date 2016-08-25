#include <limits>
#include <random>

#include "parser/parser.h"
#include "parser/wordvec.h"
#include "utils/print.h"
#include "utils/hdf5.h"
#include "utils/math.h"
#include "utils/profiling.h"

/*
-UNKNOWN-
-LPB- (
-RPB- )
*/
namespace sent2vec  {

using char_t = char;
using wcount_t = int32_t;
using idx_t = std::size_t;
using val_t = double;

auto is_unknown_widx = [](auto x){return x==std::numeric_limits<decltype(x)>::max();};
auto occurence_cutoff = [](auto x){return x>5;};


struct UnigramDist{
    using char_t =  sent2vec::char_t;
    using count_t = sent2vec::wcount_t;
    using prob_t = std::vector<double>;
    using iter_t = prob_t::const_iterator; 

    UnigramDist(util::io::H5file const &h5store, std::string voca_file, std::string count_file)
    : voca{h5store.getRawData<char_t>(util::io::H5name{voca_file})},
      count{h5store.getRawData<count_t>(util::io::H5name{count_file})},
      prob(count.size()) 
    {
        auto norm = 1.0 / std::accumulate(count.cbegin(), count.cend(), count_t{0});
        for(size_t i=0; i<count.size(); ++i) {
            prob[i]=count[i]*norm;
        }
    }
    prob_t get_probs(std::vector<idx_t> idxs){
        prob_t ps;
        for(auto idx:idxs) {
            if(is_unknown_widx(idx)) ps.push_back(-1.0);
            else ps.push_back(prob[idx]);
        }
        return ps;
    } 
    rnn::wordrep::Voca voca;
    std::vector<count_t> count;
    prob_t prob;
};

template<typename T>
class Sampler{
public:
    Sampler(T beg, T end)
    : rd{}, gen{rd()}, dist{beg, end} {}
    auto operator() () {return dist(gen);}

private:
    std::random_device rd;
    std::mt19937 gen;
    std::discrete_distribution<> dist;
};
/*
pw = cnt / pos_max < 1
x = pw/sample 
x>1 : typical 
x<1 : rare
ran = (sqrt(x) + 1) / x;
if(ran < rand_gen_double()) continue;
*/

class SubSampler{
public:
    SubSampler(val_t rate)
    : rd{}, gen{rd()}, uni01{0.0,1.0}, rate_inv{1.0/rate} {}
    template<typename TW, typename TP>
    auto operator() (TW const &widxs, TP const &probs) {
        TW widxs_subsampled;
        for(decltype(probs.size()) i=0; i<probs.size(); ++i){
            auto p_word = probs[i];
            if(p_word<0.0) continue;
            auto x = p_word*rate_inv;
            auto p = (std::sqrt(x)+1)/x;
            if(p < uni01(gen)) continue;
            widxs_subsampled.push_back(widxs[i]);
        }
        return widxs_subsampled;
    }
private:    
    std::random_device rd;
    std::mt19937 gen;
    std::uniform_real_distribution<> uni01;
    val_t rate_inv;
};


struct WordVecContext{
};
struct SentVecContext{
    using widxs_t = std::vector<idx_t> ;
    using iter_t = widxs_t::const_iterator; 
    SentVecContext(idx_t sidx, iter_t self, widxs_t const &widxs, 
                   idx_t left, idx_t right)
    : sidx{sidx}, widx{*self} {
        auto beg=widxs.cbegin();
        auto end=widxs.cend();
        assert(self<end);
        auto left_beg = self-left<beg? beg : self-left;
        auto right_end= self+1+right>end? end : self+1+right;
        std::copy(left_beg, self, std::back_inserter(left_widxs));
        std::copy(self+1, right_end, std::back_inserter(right_widxs));
    }
    idx_t sidx;
    idx_t widx;
    widxs_t left_widxs;
    widxs_t right_widxs;    
};

/*
class NegativeSampler{
public:
    NegativeSampler(idx_t left, idx_t right)
    :left{left}, right{right} {}
    SentVecContext operator() (idx_t sidx, idx_t widx){
    }
private:
    idx_t left;
    idx_t right;
};
*/

}//namespace sent2vec




using namespace rnn::simple_model;
using namespace rnn::wordrep;
using namespace util::math;
using namespace util::io;
using namespace util;
using namespace sent2vec;
void test_unigram_sampling(){
    H5file file{H5name{"data.h5"}, hdf5::FileMode::read_exist};
    UnigramDist word_dist{file, "1b.short_sents.bar.word_key", "1b.short_sents.word_count"};    
    Sampler<UnigramDist::iter_t> negative_sampler{word_dist.prob.cbegin(), word_dist.prob.cend()};
    
    std::map<int, int> m;
    for(int n=0; n<10000; ++n) {
        ++m[negative_sampler()];
    }
    for(auto p : m) {
        std::cout << p.first << " " <<word_dist.voca.getWord(p.first).val <<" generated " << p.second << " times\n";
    }
}

auto print_context=[](auto const &context, auto const &word_dist){
    for(auto idx: context.left_widxs){
        if(is_unknown_widx(idx)) {print("-UNKNOWN-"); continue;}
        print(word_dist.voca.getWord(idx).val);
    }
    print("__");
    print(word_dist.voca.getWord(context.widx).val);
    print("__");
    for(auto idx: context.right_widxs){
        if(is_unknown_widx(idx))  {print("-UNKNOWN-"); continue;}
        print(word_dist.voca.getWord(idx).val);
    }
    print('\n');
};

void test_context_words(){
    H5file file{H5name{"data.h5"}, hdf5::FileMode::read_exist};
    UnigramDist word_dist{file, "1b.short_sents.bar.word_key", "1b.short_sents.word_count"};    
    VocaIndexMap word2idx = word_dist.voca.indexing();
    
    TokenizedSentences dataset{"testset"};
    auto& lines = dataset.val;
    SubSampler sub_sampler{0.0001};
    for(size_t sidx=0; sidx<lines.size(); ++sidx){
        auto& sent = lines[sidx];
        auto widxs_orig = word2idx.getIndex(sent);
        auto pws = word_dist.get_probs(widxs_orig);
        auto widxs = sub_sampler(widxs_orig, pws);
        for(auto widx:widxs) assert(!is_unknown_widx(widx));
        for(auto self=widxs.cbegin(); self!=widxs.end(); ++self)
            print_context(SentVecContext{sidx, self, widxs, 5,5}, word_dist);
        // for(auto idx : idxs) {            
        //     if(is_unknown_widx(idx)) continue;
        //     if(!occurence_cutoff(word_dist.count[idx])) continue; 
        //     print(idx);
        //     print(" : ");
        //     print(word_dist.count[idx]);
        // }
        // print('\n');
    }

}


template<int word_dim>
WordBlock_base<word_dim> random_WordBlock(idx_t voca_size){
    //constexpr int word_dim = 100;
    std::random_device rd{};
    std::mt19937 gen{rd()};
    std::uniform_real_distribution<val_t> uni01{0.0,1.0};
    std::vector<val_t> wvec_init(voca_size*word_dim);
    for(auto &x : wvec_init) x= uni01(gen)-0.5;
    return WordBlock_base<word_dim>{wvec_init};    
}
void test_voca_update(){
    Timer timer{};
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
    voca_vecs.push_back(vec);
    span_1d<val_t,word_dim> vec3 = voca_vecs[100]; //vec3==1
    // vec+=vec2; //vec==3
    for(int i=0; i<100; ++i) vec[i]=2.5; //vec==2.5
    // assert(voca_vecs[0]==vec);
    // voca_vecs[0]+=voca_vecs[1];
    print(dot(vec, vec2));
    print(dot(voca_vecs[0], vec2));
    print(dot(vec3, vec2));
    print(":dot\n");
}

auto sigmoid=[](auto const &x, auto const &y){
    auto xy = util::math::dot(x,y);
    return  1/(1+std::exp(-xy));
};
auto sigmoid_plus=[](auto const &x, auto const &y){
    auto xy = util::math::dot(x,y);
    return  1/(1+std::exp(xy));
};

struct SparseGrad{
    SparseGrad(val_t f_wc, val_t f_wcn,
               idx_t idx_w, idx_t idx_c, idx_t idx_cn)
    : f_wc{f_wc}, f_wcn{f_wcn},
      idx_w{idx_w},idx_c{idx_c}, idx_cn{idx_cn} {}
    val_t f_wc, f_wcn;
    idx_t idx_w, idx_c, idx_cn;
};
//vocavecs_gradient_descent=[]
auto vocavecs_gradient=[](auto const &voca_vecs, 
                     auto idx_w, auto idx_c, auto idx_cn){
    auto w=voca_vecs[idx_w];
    auto c=voca_vecs[idx_c];
    auto cn=voca_vecs[idx_cn];
    //grad_w : (1-sigmoid(w,c)) *c + (sigmoid_plus(w,c)-1) * c_n
    //grad_c : (1-sigmoid(w,c)) * w 
    //grad_cn : (sigmoid_plus(w,c)-1) *w
    return SparseGrad{1-sigmoid(w,c), sigmoid_plus(w,c)-1, idx_w, idx_c, idx_cn};
};

struct VocavecsGradientDescent{
    VocavecsGradientDescent(val_t alpha)
    :alpha{alpha} {}
    template<typename T>
    void operator() (T &voca_vecs, SparseGrad const & grad){
        auto w=voca_vecs[grad.idx_w];
        auto c=voca_vecs[grad.idx_c];
        auto cn=voca_vecs[grad.idx_cn];
        // w += alpha * grad.f_wc * c + alpha * grad.f_wcn * cn;
        // c += alpha * grad.f_wc * w;
        // cn+= alpha * grad.f_wcn* w;
    }
    val_t alpha;
};

void test_grad_update(){
    Timer timer{};
    constexpr util::DataType w2vmodel_f_type = util::DataType::sp;
    constexpr int word_dim=100;

    H5file file{H5name{"data.h5"}, hdf5::FileMode::read_exist};
    UnigramDist word_dist{file, "1b.short_sents.bar.word_key", "1b.short_sents.word_count"};    
    VocaIndexMap word2idx = word_dist.voca.indexing();
    auto voca_size = word_dist.voca.size();

    timer.here_then_reset("UnigramDist constructed");
    WordBlock voca_vecs=random_WordBlock<word_dim>(voca_size);
    std::cerr << "Sum: "<<sum(voca_vecs[10]) << std::endl;
    timer.here_then_reset("Initial WordBlock constructed");

    print(voca_size);
    print(": voca_size\n");
    TokenizedSentences dataset{"testset"};
    auto& lines = dataset.val;
    for(size_t sidx=0; sidx<lines.size(); ++sidx){
        auto& sent = lines[sidx];
        auto idxs = word2idx.getIndex(sent);
        //auto gradient=get_gradient(voca_vecs, idx_w, idx_c, idx_cn);
        //voca_vecs += alpha*gradient - lambda*param;
        //voca_vecs ++ adagrad(gradient);
        auto word_block = voca_vecs.getWordVec(idxs);
        std::cerr << "Sum: "<<sum(word_block.span) << std::endl;
    }
    //auto vocafile = "wordvec.h5";
    //auto w2vmodel_dataset = "1b.model.voca";
    timer.here_then_reset("test_voca_update() is finished.");
    //WordBlock voca_vecs{load_voca_vecs(vocafile,w2vmodel_dataset,word_dim,w2vmodel_f_type)}
}

int main(){
    // test_unigram_sampling();
    test_context_words();
    test_voca_update();

    return 0;
}
