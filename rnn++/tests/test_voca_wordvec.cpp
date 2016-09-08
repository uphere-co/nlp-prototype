#include "tests/test_voca_wordvec.h"

#include "fmt/printf.h"

#include "parser/config.h"
#include "parser/parser.h"
#include "utils/print.h"
#include "utils/math.h"
#include "utils/linear_algebra.h"

using namespace rnn::simple_model;
using namespace rnn::wordrep;
using namespace rnn::config;
using namespace util;
using namespace util::math;

namespace{
void print_words(Voca const &voca){
    for(size_t i=0; i<voca.size(); ++i){
        std::cout << voca.getWord(i).val <<std::endl;
    }
}


}//nameless namespace
namespace test{

void test_collecting_new_voca(){
    VocaInfo rnn{file_name, voca_name, w2vmodel_name, w2vmodel_f_type};
    auto &voca = rnn.voca;
    auto &word2idx = rnn.word2idx;
    auto &wordvecs = rnn.voca_vecs;
    WordVectors new_words1{};
    WordVectors new_words2{};

    {Word word{"the"};
    new_words1.add_word(word, wordvecs[0]);}
    {Word word{"cat"};
    new_words1.add_word(word, wordvecs[1]);}
    {Word word{"the__1Ha51H8agj37f"};
    new_words1.add_word(word, wordvecs[2]);}
    {Word word{"cat__1Ha51H8agj37f"};
    new_words1.add_word(word, wordvecs[3]);}
    {Word word{"the__1Ha51H8agj37f"};
    new_words1.add_word(word, wordvecs[4]);}
    {Word word{"the  6zbg5afq1j37f"};
    new_words2.add_word(word, wordvecs[5]);}
    {Word word{"the"};
    new_words2.add_word(word, wordvecs[6]);}
    auto new_words = new_words1 + new_words2;    

    WordBlock new_vecs{new_words.serialize_vectors()};
    Voca new_voca{new_words.serialize_words()};
    auto new_word2idx = new_voca.indexing();
    write_to_disk(new_words, "test");
    print_words(new_voca);

    //Test if "the" was over-written in addition operation.
    assert(sum(wordvecs[6])==sum(new_vecs[new_word2idx.getIndex(Word{"the"})]));
    assert(sum(wordvecs[1])==sum(new_vecs[new_word2idx.getIndex(Word{"cat"})]));
    //Test if "the__1Ha51H8agj37f" was over-written correctly. 
    assert(sum(wordvecs[4])==sum(new_vecs[new_word2idx.getIndex(Word{"the__1Ha51H8agj37f"})]));
    assert(sum(wordvecs[3])==sum(new_vecs[new_word2idx.getIndex(Word{"cat__1Ha51H8agj37f"})]));
    assert(sum(wordvecs[5])==sum(new_vecs[new_word2idx.getIndex(Word{"the  6zbg5afq1j37f"})]));
    assert(new_voca.size()==new_vecs.size());
}

}//namespace test

