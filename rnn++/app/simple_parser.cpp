#include<iostream>
#include<cassert>

#include"utils/hdf5.h"
#include"parser/voca.h"

using namespace rnn::io;
using namespace rnn::parser::wordrep;
namespace rnn_t = rnn::type;
// class VocaRep{
//     VocaRep(std::vector<)
// };

int main(){
    try {
        H5name file_name{"data.h5"};
        H5name voca_name{"1b.model.voca"}, w2vmodel_name{"1b.model"};
        //voca_max_word_len can be read using `h5dump -H` command.
        //It can be directly read from a H5File,
        //but it needs knowledge of low level details of HDF5.
        int voca_max_word_len = 74;
        //voca_size, word_dim are easy to get programmatically.
        //For consistencies, however, they are set by runtime configuration.
        int voca_size=552402;
        int word_dim=100;

        H5file file{file_name};
        Voca voca{file.getRawData<rnn_t::char_t>(voca_name), voca_max_word_len};

        VocaIndexMap word2idx = voca.indexing();
        for(size_t i=0; i<voca.size(); ++i){
            if(i>100) break;
            assert(word2idx.getIndex(voca.getWord(i)) == i);
        }
        for(size_t i=0; i<voca.size(); ++i){
            std::cout << voca.getWord(i) <<std::endl;
        }

        VocaRep voca_vecs{file.getRawData<float_type>(w2vmodel_name), voca_size,word_dim};
        int j=0;
        for(auto it=std::cbegin(voca_vecs.val); it!=std::cend(voca_vecs.val); ++it){
           std::cerr << *it << "\t"<< voca_vecs.val[j] << " " << *(it+100)<< " " << voca_vecs.val[j+100] << " "<<std::endl;
           ++j;
           if(j>10) break;
        }
    } catch (H5::Exception ex) {
        std::cerr << ex.getCDetailMsg() << std::endl;
    } catch (...) {
        std::cerr << "Unknown exception" << std::endl;
    }
    static_assert(std::is_nothrow_destructible<H5file>::value == true, "");

    return 0;
}
//
