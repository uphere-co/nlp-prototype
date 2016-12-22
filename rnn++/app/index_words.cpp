#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <cassert>
#include <cctype>
#include <iterator>
#include <random>

#include <xxhashct/xxh64.hpp>
#include <fmt/printf.h>

#include "wordrep/word_uid.h"

#include "utils/string.h"
#include "utils/json.h"
#include "utils/profiling.h"
#include "utils/span.h"
#include "utils/parallel_algorithm.h"

using wordrep::WordUIDindex;
using util::Timer;

using util::binary_find;
using util::to_map;
using util::to_sorted_pairs;

template<typename T>
auto hash(T* ptr, size_t len){
    return xxh64::hash(reinterpret_cast<const char*>(ptr), len, 113377);
}

template<typename KEY>
struct TokenHash{
    template<typename T>
    KEY operator() (std::string const &text, T text_beg, T beg, T end) const;
};
template<>
struct TokenHash<std::string>{
    template<typename T>
    std::string operator() (std::string const &text, T text_beg, T beg, T end) const {
        return text.substr(beg-text_beg, end-beg);
    }
    std::string operator() (std::string const &word) const {
        return word;
    }
};

template<>
struct TokenHash<wordrep::WordUID>{
    using WordUID = wordrep::WordUID;
    template<typename T>
    WordUID  operator() (std::string const &text, T text_beg, T beg, T end) const {
        return WordUID::from_unsigned(hash(text.data()+(beg-text_beg), end-beg));
    }
    WordUID  operator() (std::string const &word) const {
        assert(word.size());
        return WordUID::from_unsigned(hash(word.data(), std::distance(word.cbegin(),word.cend())));
    }
};

template<typename KEY>
struct WordIterBase{
    using key_type    = KEY;
    using hasher_type = TokenHash<KEY>;
    WordIterBase(std::string text)
            : text_strs{std::move(text)}
    {}
    template<typename OP>
    void iter(OP const &op) const {
        auto text_beg = std::cbegin(text_strs);
        auto text_end = std::cend(text_strs);
        auto beg = std::find_if_not(text_beg, text_end, [](auto x){return std::isspace(x);});
        while(beg!=text_end){
            auto end=std::find_if(beg, text_end, [](auto x){return std::isspace(x);});
            auto word = get_hash(text_strs, text_beg, beg, end);
            op(word);
            if(end==text_end) break;
            beg = std::find_if_not(end, text_end, [](auto x){return std::isspace(x);});
        }
    }

    hasher_type get_hash{};
    std::string text_strs;
};

using WordIter=WordIterBase<wordrep::WordUID>;
//using WordIter=WordIterBase<std::string>;
using count_t = std::map<WordIter::key_type, size_t>;

template<typename T>
std::optional<std::vector<char>> read_chunk(T &is, int64_t n_buf){
    std::vector<char> buffer(n_buf);
    is.read(buffer.data(), buffer.size());
    if(!is.gcount()) return {};
    if(is.gcount()==n_buf){
        for(char c=is.get(); is&&c!='\n'; c=is.get())
            buffer.push_back(c);
    }
    buffer.push_back('\0');
    return buffer;
}


class WordCounter{
public:
    using count_type = count_t;
    using key_type   = count_t::key_type;
    using mapped_type = count_t::mapped_type;
    using map_t = tbb::concurrent_hash_map<key_type,mapped_type,util::TBBHashCompare<key_type>>;
    void count(std::string str){
        WordIter text{std::move(str)};
        count_type word_counts;
        text.iter([&word_counts](auto& word) {++word_counts[word];});
        for(auto const& elm : word_counts){
            map_t::accessor a;
            wcs.insert(a, elm.first);
            a->second += elm.second;
        }
    }
    std::map<key_type,mapped_type >
    to_map() const { return ::to_map(wcs); };
    //std::pair<std::vector<key_type>,std::vector<mapped_type>>
    std::vector<std::pair<key_type,mapped_type>>
    to_pairs() const { return ::to_sorted_pairs(wcs);};

private:
    map_t wcs;
};


template<typename T>
auto word_count(T&& is){
    WordCounter counter;
    tbb::task_group g;
    Timer timer;
    while (auto buffer=read_chunk(is, 200000)) {
        //auto ptr = std::make_unique<std::vector<char>>(buffer.value());
        std::string str{buffer.value().data()};
        g.run([&counter,str{std::move(str)}]() { //important to copy the str variable.
            counter.count(str);
        });
    }
    g.wait();
    timer.here_then_reset("Word counting is finished.");
    return counter.to_pairs();
//    return counter.to_map();
}

namespace test{

void string_iterator(){
    WordIterBase<std::string> text{"11 22 33\t\t14 15\n16 17 18   119\t \n 11110\n"};
    std::vector<int64_t> xs{11,22,33,14,15,16,17,18,119,11110};

    std::vector<int64_t> tokens;
    text.iter([&tokens](auto& word){tokens.push_back(std::stoi(word));});
    assert(tokens.size()==xs.size());
    for(auto pair : util::zip(xs,tokens)) assert(pair.first==pair.second);
}

std::string strip(std::string const& str){
    auto beg=std::find_if_not(str.cbegin(), str.cend(), [](auto x){return std::isspace(x);});
    auto end=std::find_if_not(str.crbegin(), str.crend(), [](auto x){return std::isspace(x);});
    return str.substr(beg-str.cbegin(), end.base()-beg);
}

void benchmark(){
    util::Timer timer{};
    auto lines = util::string::readlines("../rnn++/tests/data/sentence.2.corenlp");
    timer.here_then_reset("Finish file readlines.");


    count_t word_counts;
    for(auto &line : lines){
        WordIter text{line};
        text.iter([&word_counts](auto& word){++word_counts[word];});
    }
    timer.here_then_reset("Finish word count / excluding file reading.");
    auto word_counts2 = word_count(std::fstream{"../rnn++/tests/data/sentence.2.corenlp"});
    timer.here_then_reset("Finish parallel word count / including file reading.");

    typename WordIter::hasher_type hasher{};
    count_t word_counts0;
    for(auto &line : lines){
        auto stripped_line = strip(line);
        assert(!stripped_line.empty());
        for(auto&& word : util::string::split(stripped_line, " ")){
            ++word_counts0[hasher(word)];
        }
    }
    timer.here_then_reset("Finish word count.");
    for(auto elm : word_counts)  assert(word_counts0[elm.first]==elm.second);
    assert(word_counts.size()==word_counts0.size());
    for(auto elm : word_counts2) assert(word_counts0[elm.first]==elm.second);
    assert(word_counts2.size()==word_counts0.size());
}

void reverse_iterator(){
    std::vector<char> buffer(100);
    std::string str = "12 34 56 78";
    for(size_t i=0; i<str.size(); ++i) buffer[i]=str[i];
    auto end=std::find_if(buffer.crbegin(), buffer.crend(), [](auto x){return x!='\0';});
    assert(end!=buffer.crbegin());
    assert(end-buffer.crend() == std::distance(str.cend(),str.cbegin()));
    auto it=std::find_if(buffer.crbegin(), buffer.crend(), [](auto x){return std::isspace(x);});
    assert(it-end==2);
}

void hash(){
    auto seed = 1;
    char cs[10] = "Hello";

    uint64_t hash = xxh64::hash (reinterpret_cast<const char*> (cs), 10, seed);
    assert(sizeof(cs)==10);
    fmt::print("{}\n", hash);
}

void uint_to_int(){
    auto umax = std::numeric_limits<uint64_t>::max();
    auto max = std::numeric_limits<int64_t>::max();
    assert(util::to_signed_positive<int64_t>(umax) == 1);
    assert(util::to_signed_positive<int64_t>(umax-10) == 11);
    uint64_t n = max;
    assert(util::to_signed_positive<int64_t>(n)==max);
}


void binary_find_check(){
    std::vector<int> vs = {1,2,3,4, 6,7,8,9};
    assert(binary_find(vs, 4).value()-vs.begin()==3);
    assert(binary_find(vs, 1).value()-vs.begin()==0);
    assert(binary_find(vs, 9).value()-vs.begin()==7);
    assert(!binary_find(vs, 0));
    assert(!binary_find(vs, 5));
    assert(!binary_find(vs, 10));
}

template<typename TK, typename TV>
TV get_val(std::vector<std::pair<TK,TV>> const &pairs, TK key){
    return binary_find(pairs,
                       [key](auto elm){return elm.first==key;},
                       [key](auto elm){return elm.first<key;}).value()->second;
};
void binary_find_benchmark(){
    using wordrep::WordUID;
    std::random_device rd{};
    std::mt19937 e{rd()};
    std::uniform_int_distribution<int64_t> ran_int{-1, 0xffffffff};
    std::uniform_int_distribution<uint64_t> ran_uint{0, 0xffffffff};

    std::vector<WordUID> keys;
    std::map<WordUID,size_t> count;
    for(int i=0; i<200000; ++i) {
        auto key = WordUID{ran_int(e)};
        keys.push_back(key);
        count[key] =  ran_uint(e);
    }
    Timer timer{};

    auto count_pairs = to_pairs(count);
    for(auto key : keys){
        assert(count[key] == get_val(count_pairs, key));
    }
    timer.here_then_reset("Passed consistancy check.");
    auto sum0=0;
    for(auto key : keys) sum0 += count[key];
    timer.here_then_reset("Iter std::map.");
    auto sum1=0;
    for(auto key : keys) sum1 += get_val(count_pairs, key);
    timer.here_then_reset("Iter std::vector<std::map>.");
    assert(sum0==sum1);
}
}//namespace test

int main(int argc, char** argv){
//    test::reverse_iterator();
//    test::string_iterator();
//    test::benchmark();
//    test::hash();
//    test::uint_to_int();
    test::binary_find_check();
    test::binary_find_benchmark();
    return 0;
    assert(argc>1);
    auto config = util::load_json(argv[1]);
    WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};

    util::Timer timer{};
    auto word_counts = word_count(std::cin);
    timer.here_then_reset("Finish word count.");
    for(auto elm : word_counts)
        fmt::print(std::cout, "{} {}\n", elm.first, elm.second);
    return 0;
}
