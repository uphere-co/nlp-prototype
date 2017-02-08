#include "utils/string.h"

#include <sstream>
#include <fstream>
#include <algorithm>
#include <functional>
#include <codecvt>
#include <locale>
#include <cctype>


#include <boost/algorithm/string.hpp>

namespace util{
namespace string{

std::vector<std::string> split(std::string words, const char *delim){
    std::vector<std::string> tokens;
    boost::split(tokens, words, boost::is_any_of(delim));
    return tokens;
}

std::string join(std::vector<std::string> words, std::string delim){
    std::stringstream s;
    auto n = words.size();
    for(decltype(n)i=0; i!=n; ++i){
        s<<words[i];
        if(i!=n-1) s<<delim;
    }
    return s.str();
}
std::string strip(std::string const& str){
    auto beg=std::find_if_not(str.cbegin(), str.cend(), [](auto x){return std::isspace(x);});
    auto end=std::find_if_not(str.crbegin(), str.crend(), [](auto x){return std::isspace(x);});
    return str.substr(beg-str.cbegin(), end.base()-beg);
}
std::string tolower(std::string str){
    std::transform(str.begin(), str.end(), str.begin(), ::tolower);
    return str;
}

std::vector<std::string> readlines(std::string file){
    std::ifstream is{file};
    return readlines(std::move(is));
}
std::vector<std::string> readlines(std::istream &&is){
    std::vector<std::string> lines;
    std::string line;
    while(std::getline(is, line)) lines.push_back(line);
    return lines;
}

std::string read_whole(std::string file){
    std::ifstream t(file);
    std::stringstream buffer;
    buffer << t.rdbuf();
    return buffer.str();
}

std::vector<char> pack_words(std::vector<std::string> const &words){
    std::vector<char> vec;
    for(auto const &x:words){
        std::copy(x.cbegin(),x.cend(),std::back_inserter(vec));
        vec.push_back('\0');
    }
    return vec;
}
std::vector<char> pack_words(std::vector<const char*> const &words){
    std::vector<char> vec;
    for(auto const &word:words){
        std::string x{word};
        std::copy(x.cbegin(),x.cend(),std::back_inserter(vec));
        vec.push_back('\0');
    }
    return vec;
}

std::vector<std::string> unpack_words(std::vector<char> const &concat_words){
    std::vector<std::string> words;
    auto it =concat_words.cbegin();
    auto end=concat_words.cend();
    while(it!=end){
        words.push_back(std::string{&(*it)});
        it=std::find(it, end, '\0');
        it=std::find_if_not(it, end, [](auto x){return x=='\0';});
    }
    return words;
}

std::vector<const char *> unpack_word_views(std::vector<char> const &concat_words){
    std::vector<const char *> words;
    auto it =concat_words.cbegin();
    auto end=concat_words.cend();
    while(it!=end){
        words.push_back(&(*it));
        it=std::find(it, end, '\0');
        it=std::find_if_not(it, end, [](auto x){return x=='\0';});
    }
    return words;
}

std::vector<util::cstring_span<>> unpack_tokenized_sentence(util::cstring_span<> sentence){
    util::cstring_span<> sent{sentence};
    std::vector<util::cstring_span<>> words;
    auto beg = sent.cbegin();
    auto end = sent.cend();
    auto it=beg;
    while(it!=end){
        auto word_beg=it-beg;
        it=std::find(it, end, ' ');
        auto word_end=it-beg;
        words.push_back(sent.subspan(word_beg, word_end-word_beg));
        it=std::find_if_not(it, end, [](auto x){return x==' ';});
    }
    return words;
}

std::string substring_unicode_offset(std::string str, int beg, int end){
    std::wstring_convert<std::codecvt_utf8<wchar_t>> to_utf8;
    std::wstring wstr = to_utf8.from_bytes(str);
    auto wsubstr = wstr.substr(beg, end-beg);
    auto substr = to_utf8.to_bytes(wsubstr);
    return substr;
}

}//namespace util::string
}//namespace util
