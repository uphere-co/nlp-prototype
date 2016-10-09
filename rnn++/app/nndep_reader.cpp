#include <vector>
#include <algorithm>

#include "fmt/printf.h"

#include "utils/json.h"
#include "utils/hdf5.h"
#include "utils/span.h"
#include "utils/string.h"

using namespace util::io;

int main(){
    H5file file{H5name{"news.Google.h5"}, hdf5::FileMode::read_exist};
    auto sent_idx = file.getRawData<int64_t>(H5name{"sent_idx"});
    auto word_raw = file.getRawData<char>(H5name{"word"});
    auto word = util::string::unpack_word_views(word_raw);
    auto idx_word = file.getRawData<int64_t>(H5name{"idx_word"});
    auto head_word_raw = file.getRawData<char>(H5name{"head_word"});
    auto head_word = util::string::unpack_word_views(head_word_raw);
    auto idx_head = file.getRawData<int64_t>(H5name{"idx_head"});
    auto pos_raw = file.getRawData<char>(H5name{"POS"});
    auto pos = util::string::unpack_word_views(pos_raw);


    auto beg=sent_idx.cbegin();
    auto end=sent_idx.cend();
    std::vector<int64_t> sent_beg{0};
    auto it=beg;
    while(it!=end) {
        it = std::find_if_not(it, end, [it](auto x) { return x == *it; });
        sent_beg.push_back(it-beg);
    }

    for(int64_t is=0; is<sent_beg.size()-1; ++is){
        auto beg=sent_beg[is];
        auto end=sent_beg[is+1];
        for(auto i=beg; i<end; ++i) {
            fmt::print("{} : ", i);
            fmt::print("{:<10} {:<2}  {:<10} {:<2}  {}\n", word[i], idx_word[i], head_word[i], idx_head[i], pos[i]);
        }
        fmt::print("{}\n",end-beg);
    }

    return 0;
}