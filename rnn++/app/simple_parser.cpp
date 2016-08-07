#include <iostream>
#include <cassert>
#include <algorithm> //forward path
#include <chrono> //profiling

#include "utils/hdf5.h"
#include "utils/math.h"
#include "utils/linear_algebra.h"
#include "parser/voca.h"
#include "parser/wordvec.h"
#include "parser/simple_model.h"
#include "parser/config.h"



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
    rnn_model::Param param = rnn_model::deserializeParam(param_raw);
    std::cerr << "Test:   3.248616=="<< sum(param.w_left.span)+sum(param.w_right.span) << std::endl;
    std::cerr << "Test: -50.581345=="<< sum(param.bias.span) << std::endl;
    std::cerr << "Test:  -0.190589=="<< sum(param.u_score.span) << std::endl;
    std::cerr << "Test:   0.675059=="<<dotdot(param.u_score, param.w_left, param.bias) << std::endl;
    std::cerr << "Test:   1.5551  =="<< dotdot(param.u_score, param.w_right, param.bias) << std::endl;
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
        auto param = rnn_model::deserializeParam(param_raw);

        H5file file{file_name, hdf5::FileMode::read_exist};
        Voca voca{file.getRawData<rnn_t::char_t>(voca_name), voca_max_word_len};
        WordBlock voca_vecs{file.getRawData<rnn_t::float_t>(w2vmodel_name), word_dim};
        VocaIndexMap word2idx = voca.indexing();

        auto t_start = std::chrono::high_resolution_clock::now();

        auto sentence = u8"A symbol of\tBritish pound is £ .";
        auto idxs = word2idx.getIndex(sentence);
        auto word_block = voca_vecs.getWordVec(idxs);
        std::cerr << sum(word_block.span) << std::endl;

        auto t_load = std::chrono::high_resolution_clock::now();

        {
            auto t_start = std::chrono::high_resolution_clock::now();
            std::vector<float> test(100000);
            auto t_end = std::chrono::high_resolution_clock::now();
            std::cerr << "Allocate vector of size "<<test.size()<<" : "<< std::chrono::duration<double, std::milli>(t_end-t_start).count() << std::endl;
        }

        auto merge_to_phrase=[&](auto const word_left, auto const word_right){
            return rnn_model::compute::merge_to_phrase(
                        param.w_left, param.w_right, param.bias,
                        word_left,word_right);
        };
        // auto phrase = merge_to_phrase(word_block[2],word_block[3]);
        // std::cerr <<"Test: 58.3763     =="<<dot(word_block[2], word_block[3])<<std::endl;
        // std::cerr <<"Test:  0.98579389 == "<< dot(param.u_score.span, phrase.span) << std::endl;
        using word_t = decltype(param.bias);
        std::vector<word_t> phrases;//WordBlock{word_block.word_dim};
        std::vector<WordBlock::wordspan_t> current_words;
        std::vector<rnn_t::float_t> scores;
        std::vector<rnn_t::idx_t> idx;


        auto n=word_block.size();
        for(decltype(n) i=0; i<n; ++i)
            current_words.push_back(word_block[i]);
        for(decltype(n) i=0; i<n-1; ++i){
            auto phrase = merge_to_phrase(word_block[i],word_block[i+1]);
            phrases.push_back(std::move(phrase));
            idx.push_back(i);
        }
        for(auto i:idx)
            scores.push_back(dot(param.u_score.span, phrases[i].span));
        while(idx.size()){
            auto max_score=std::max_element(scores.cbegin(), scores.cend());

            // for(auto const x: current_words)
            //     std::cerr<<sum(x) << " ";
            // std::cerr<<std::endl;

            //i : score index
            //idx : word_index
            auto i_max = max_score-scores.cbegin();
            auto idx_max = idx[i_max];
            // std::cerr<<current_words.size()<<" Score: "<<*max_score<<", i_max : "<<i_max<<", idx_max : "<<idx_max<<"\n";
            auto new_word=phrases[idx_max];
            current_words[i_max]=new_word.span;
            std::copy(current_words.cbegin()+i_max+2, current_words.cend(), current_words.begin()+i_max+1);
            current_words.pop_back();
            if(max_score!=scores.cbegin()){
                auto i_left = i_max-1;
                auto idx_left=idx[i_left];
                auto new_phrase = merge_to_phrase(current_words[i_left], new_word.span);
                phrases[idx_left]=std::move(new_phrase);
                scores[i_left] =dot(param.u_score.span, new_phrase.span);
                // std::cerr<<idx_left<<" New left score : " << scores[i_left]<<"\n";
            }
            if(max_score!=scores.cend()-1){
                auto i_right = i_max+1;
                auto idx_right=idx[i_right];
                auto new_phrase = merge_to_phrase(new_word.span, current_words[i_right]);
                phrases[idx_right]=std::move(new_phrase);
                scores[i_right] =dot(param.u_score.span, new_phrase.span);
                // std::cerr<<idx_right<<" New right score : " << scores[i_right]<<"\n";
            }
            std::copy(idx.cbegin()+i_max+1, idx.cend(), idx.begin()+i_max);
            idx.pop_back();
            std::copy(scores.cbegin()+i_max+1, scores.cend(), scores.begin()+i_max);
            scores.pop_back();
            // std::cerr<<"Phrase remains :";
            // for(auto i : idx)
            //     std::cerr<<i<< " ";
            // std::cerr<<std::endl;
            word_block.push_back(new_word.span);
            std::cerr<<dot(param.u_score.span, word_block[word_block.size()-1])<<std::endl;
        }
        auto t_end = std::chrono::high_resolution_clock::now();
        std::cout << "Wall time: "<< std::chrono::duration<double, std::milli>(t_load-t_start).count() << std::endl;
        std::cout << "Wall time: "<< std::chrono::duration<double, std::milli>(t_end-t_load).count() << std::endl;
    } catch (H5::Exception ex) {
        std::cerr << ex.getCDetailMsg() << std::endl;
    } catch (...) {
        std::cerr << "Unknown exception" << std::endl;
    }
    static_assert(std::is_nothrow_destructible<H5file>::value == true, "");
    static_assert(sizeof(WordBlock::idx_t) == 8, "");

    return 0;
}
