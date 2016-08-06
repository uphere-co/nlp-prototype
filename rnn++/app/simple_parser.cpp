#include<iostream>
#include<cassert>

#include"utils/hdf5.h"
#include"utils/math.h"
#include"utils/linear_algebra.h"
#include"parser/voca.h"
#include"parser/wordvec.h"
#include"parser/simple_model.h"
#include"parser/config.h"



using namespace util::io;
using namespace util::math;
using namespace rnn::parser::wordrep;
using namespace rnn::config;
namespace rnn_t = rnn::type;

namespace{
void test_voca_index(Voca const &voca, VocaIndexMap const &word2idx){
    for(size_t i=0; i<voca.size(); ++i){
        if(i>100) break;
        assert(word2idx.getIndex(voca.getWord(i)) == i);
    }
    for(size_t i=0; i<voca.size(); ++i){
        std::cout << voca.getWord(i) <<std::endl;
    }
}
}//nameless namespace

void test_init_rnn(){
    /*
    -148.346
    ((((A (symbol (of British))) pound) is) (£ .))
    [2, 1, 0, 0, 0, 1, 0]
    Scores     :  [ 0.98579389  0.98854256  0.99147892  0.97225118  0.97717702  0.94854963
      0.9967401 ]
    Block shape:  (15, 100)
    (100, 200)
    W=3.248616 b=-50.581345 u=-0.190589
    u⋅W_left⋅b=0.675059
    u⋅W_right⋅b=1.5551
    */

    namespace rnn_model = rnn::simple_model;
    H5file param_storage{rnn_param_store_name, hdf5::FileMode::read_exist};
    auto param_raw = param_storage.getRawData<rnn_t::float_t>(rnn_param_name);
    // auto span = gsl::span<rnn_t::float_t>{param_raw};
    rnn_model::Param<word_dim> param = rnn_model::deserializeParam<word_dim>(param_raw);
    std::cerr << sum(param.w_left.span)+sum(param.w_right.span) << std::endl;
    std::cerr << sum(param.bias.span) << std::endl;
    std::cerr << sum(param.u_score.span) << std::endl;
    std::cerr << dotdot(param.u_score, param.w_left, param.bias) << std::endl;
    std::cerr << dotdot(param.u_score, param.w_right, param.bias) << std::endl;
    // auto w_flat = gsl::as_span(w.data(), w.extent(0)*w.extent(1));
}
void test_read_voca(){
    H5file file{file_name, hdf5::FileMode::read_exist};
    Voca voca{file.getRawData<rnn_t::char_t>(voca_name), voca_max_word_len};
    WordBlock voca_vecs{file.getRawData<rnn_t::float_t>(w2vmodel_name), word_dim};
    std::cerr << voca_vecs.size() << " " << voca_size <<std::endl;
    VocaIndexMap word2idx = voca.indexing();

    test_voca_index(voca, word2idx);
    auto sentence = u8"A symbol of\tBritish pound is £ .";
    auto idxs = word2idx.getIndex(sentence);
    auto word_block = voca_vecs.getWordVec(idxs);

    std::cerr << sum(word_block.span) << std::endl;
}
int main(){
    try {
        test_init_rnn();
        // test_read_voca();
        namespace rnn_model = rnn::simple_model;

        H5file param_storage{rnn_param_store_name, hdf5::FileMode::read_exist};
        auto param_raw = param_storage.getRawData<rnn_t::float_t>(rnn_param_name);
        auto param = rnn_model::deserializeParam<word_dim>(param_raw);

        H5file file{file_name, hdf5::FileMode::read_exist};
        Voca voca{file.getRawData<rnn_t::char_t>(voca_name), voca_max_word_len};
        WordBlock voca_vecs{file.getRawData<rnn_t::float_t>(w2vmodel_name), word_dim};
        VocaIndexMap word2idx = voca.indexing();

        auto sentence = u8"A symbol of\tBritish pound is £ .";
        auto idxs = word2idx.getIndex(sentence);
        auto word_block = voca_vecs.getWordVec(idxs);
        std::cerr << sum(word_block.span) << std::endl;

        using rnn_model::compute::merge_to_phrase;
        auto phrase = merge_to_phrase(param.w_left, param.w_right, param.bias,
                                      word_block.getWordVec(2),word_block.getWordVec(3));
        std::cerr <<"58.3763 =="<<dot(word_block[2], word_block[3])<<std::endl;
        std::cerr <<"0.98579389 == "<< dot(param.u_score.span, phrase.span) << std::endl;
    } catch (H5::Exception ex) {
        std::cerr << ex.getCDetailMsg() << std::endl;
    } catch (...) {
        std::cerr << "Unknown exception" << std::endl;
    }
    static_assert(std::is_nothrow_destructible<H5file>::value == true, "");
    static_assert(sizeof(WordBlock::idx_t) == 8, "");

    return 0;
}
