#include<iostream>
#include<cassert>

#include"utils/hdf5.h"
#include"parser/voca.h"

using namespace rnn::io;
using namespace rnn::parser;

int main(){
   H5name file_name{"data.h5"};
   H5name voca_name{"1b.model.voca"}, w2vmodel_name{"1b.model"};
   H5file file{file_name};
   //voca_max_word_len can be read using `h5dump -H` command.
   //It can be directly read from a H5File,
   //but it needs knowledge of low level details of HDF5.
   int voca_max_word_len = 74;
   Voca voca{file.getRawData<char>(voca_name), voca_max_word_len};
   for(int i=0; i<voca.size(); ++i){
      std::string word = voca.getWord(i);
      std::cout << word <<std::endl;
   }

   return 0;
}
//
