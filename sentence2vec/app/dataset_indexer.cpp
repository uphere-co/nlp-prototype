#include <limits>
#include "parser/parser.h"
#include "utils/print.h"
#include "utils/hdf5.h"

/*
-UNKNOWN-
-LPB- (
-RPB- )
*/
namespace sent2vec  {

using char_t = char;
using wcount_t = int32_t;

}

auto is_unknown_widx = [](auto x){return x==std::numeric_limits<decltype(x)>::max();};
auto occurence_cutoff = [](auto x){return x>5;};

int main(){
    using namespace rnn::simple_model;
    using namespace rnn::wordrep;
    using namespace util::io;
    using namespace util;
    using namespace sent2vec;

    H5file file{H5name{"data.h5"}, hdf5::FileMode::read_exist};
    Voca voca{file.getRawData<char_t>(H5name{"1b.short_sents.bar.word_key"})};
    auto wcounts = file.getRawData<wcount_t>(H5name{"1b.short_sents.word_count"});
    
    VocaIndexMap word2idx = voca.indexing();
    
    TokenizedSentences dataset{"testset"};
    auto& lines = dataset.val;
    for(size_t sidx=0; sidx<lines.size(); ++sidx){
        auto& sent = lines[sidx];
        auto idxs = word2idx.getIndex(sent);

        for(auto idx : idxs) {            
            if(is_unknown_widx(idx)) continue;
            if(!occurence_cutoff(wcounts[idx])) continue; 
            print(idx);
            print(" : ");
            print(wcounts[idx]);
        }
        print('\n');
    }

    return 0;
}
