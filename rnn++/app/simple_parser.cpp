#include<iostream>
#include<cassert>

#include"utils/hdf5.h"
#include"utils/math.h"
#include"parser/voca.h"

namespace rnn{
namespace config{
util::io::H5name file_name{"data.h5"};
util::io::H5name voca_name{"1b.model.voca"};
util::io::H5name w2vmodel_name{"1b.model"};

//voca_max_word_len can be read using `h5dump -H` command.
//It can be directly read from a H5File,
//but it needs knowledge of low level details of HDF5.
int voca_max_word_len = 74;
//voca_size, word_dim are easy to get programmatically.
//For consistencies, however, they are set by runtime configuration.
size_t voca_size=552402;
int word_dim=100;
}//namescpae config
}//namespace rnn


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

int main(){
    try {
        H5file file{file_name};
        Voca voca{file.getRawData<rnn_t::char_t>(voca_name), voca_max_word_len};
        WordBlock voca_vecs{file.getRawData<rnn_t::float_t>(w2vmodel_name), word_dim};
        std::cerr << voca_vecs.size() << " " << voca_size <<std::endl;
        VocaIndexMap word2idx = voca.indexing();

        test_voca_index(voca, word2idx);
        auto sentence = u8"A symbol of\tBritish pound is £ .";
        auto idxs = word2idx.getIndex(sentence);
        auto word_block = voca_vecs.getWordVec(idxs);
        std::cerr << sum(word_block.span) << std::endl;

    } catch (H5::Exception ex) {
        std::cerr << ex.getCDetailMsg() << std::endl;
    } catch (...) {
        std::cerr << "Unknown exception" << std::endl;
    }
    static_assert(std::is_nothrow_destructible<H5file>::value == true, "");
    static_assert(sizeof(WordBlock::idx_t) == 8, "");

    return 0;
}
