#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <cassert>
#include <cctype>
#include <iterator>
#include <random>

#include <fmt/printf.h>

#include "wordrep/word_uid.h"
#include "wordrep/word_hash.h"
#include "wordrep/word_iter.h"
#include "wordrep/word_count.h"
#include "wordrep/word_prob.h"
#include "wordrep/voca.h"
#include "wordrep/indexes.h"

#include "similarity/config.h"

#include "utils/math.h"
#include "utils/string.h"
#include "utils/json.h"
#include "utils/profiling.h"
#include "utils/span.h"
#include "utils/parallel_algorithm.h"
#include "utils/random.h"
#include "utils/versioned_name.h"
#include "utils/hdf5.h"
#include "utils/persistent_vector.h"


using wordrep::ChunkIndex;
using wordrep::SentUID;
using wordrep::WordUID;
using wordrep::VocaIndex;
using wordrep::WordUIDindex;
using WordCounter = wordrep::WordCounter<WordUID>;

using util::Timer;

using util::binary_find_cell;
using util::binary_find;
using util::to_map;
using util::map;
using util::zip;
using util::apply;
using util::to_sorted_pairs;
using util::filter;
using util::filter_inplace;
using util::string::strip;





namespace test{

void string_iterator(){
    wordrep::WordIterBase<std::string> text{"11\r22\r\r\r33\t\t14 15\r\n16 17 18   119\t \n 11110\n"};
    std::vector<int64_t> xs{11,22,33,14,15,16,17,18,119,11110};

    std::vector<int64_t> tokens;
    text.iter([&tokens](auto& word){tokens.push_back(std::stoi(word));});
    assert(tokens.size()==xs.size());
    for(auto pair : util::zip(xs,tokens)) assert(pair.first==pair.second);
}

void benchmark(){
    util::Timer timer{};
    auto filename = "../rnn++/tests/data/sentence.2.corenlp";
//    auto filename = "news.2014.train";

    WordCounter word_count;
    auto word_counts = word_count.count(std::fstream{filename});
    timer.here_then_reset("Finish parallel word count / including file reading.");

    auto lines = util::string::readlines(filename);
    timer.here_then_reset("Finish file readlines.");
    WordCounter::count_type word_counts_serial;
    for(auto &line : lines){
        WordCounter::WordIter text{line};
        text.iter([&word_counts_serial](auto& word){++word_counts_serial[word];});
    }
    timer.here_then_reset("Finish word count / excluding file reading.");


    typename WordCounter::WordIter::hasher_type hasher{};
    WordCounter::count_type word_counts_simple;
    for(auto &line : lines){
        auto stripped_line = strip(line);
        assert(!stripped_line.empty());
        for(auto&& word : util::string::split(stripped_line, " ")){
            ++word_counts_simple[hasher(word)];
        }
    }
    timer.here_then_reset("Finish word count.");
    for(auto elm : word_counts)  assert(word_counts_simple[elm.first]==elm.second);
    assert(word_counts.size()==word_counts_simple.size());
    for(auto elm : word_counts_serial) assert(word_counts_simple[elm.first]==elm.second);
    assert(word_counts_serial.size()==word_counts_simple.size());
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
    char cs[10] = "Hello";
    assert(sizeof(cs)==10);

    uint64_t hash = xxh64::hash(reinterpret_cast<const char*> (cs), 10, util::xxh64_seed);
    fmt::print("With seed {}, hash of '{}' : {}\n", util::xxh64_seed, cs, hash);
    std::string str = "Hello";
    assert(hash==util::hash(cs, 10));
    {
        std::string str{"Test string."};
        std::string str2{"Test string!"};
        assert(util::hash(str)!=util::hash(str2));
    }
}
void hash_composite(){
    struct Foo{
        WordUID x;
        WordUID y;
    };

    auto x = std::make_pair<WordUID::val_t,WordUID::val_t>(1,1);
    auto y = std::make_pair<WordUID,WordUID>(1,1);
    auto z = std::make_pair<WordUID,WordUID>(1,1);
    assert(&y!=&z);
    assert(util::hash(x)==util::hash(y));
    assert(util::hash(y)==util::hash(z));
    Foo xx{1,1};
    assert(util::hash(x)==util::hash(xx));
    fmt::print("{}\n", util::hash(x));

    std::vector<WordUID> xs{1,2,3,4,5,6,7};
    std::vector<WordUID::val_t> ys{1,2,3,4,5,6,7};
    std::vector<WordUID> xs1{1,2,3,4,5,6,7,8};
    std::vector<WordUID> xs2{1,2,3,4,5,6,6};
    assert(util::hash(xs)==util::hash(ys));
    assert(util::hash(xs)!=util::hash(xs1));
    assert(util::hash(xs)!=util::hash(xs2));
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


std::map<WordUID, size_t> random_word_counts(int n) {
    std::random_device rd{};
    std::mt19937 e{rd()};
    std::uniform_int_distribution<int64_t> ran_int{-1, 0xffffffff};
    std::uniform_int_distribution<uint64_t> ran_uint{0, 0xffffffff};

    std::map<WordUID, size_t> count;
    for (int i = 0; i !=n; ++i) {
        auto key = WordUID{ran_int(e)};
        count[key] = ran_uint(e);
    }
    return count;
}
void binary_find_benchmark(){
    auto count = random_word_counts(500000);
    std::vector<WordUID> keys = get_keys(count);
    Timer timer{};

    auto count_pairs = to_sorted_pairs(count);
    assert(count_pairs.front().first<count_pairs.back().first);
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

void container_filter(){
    std::vector<int> vs{1,3,4,5,6};
    auto fvs = filter(vs, [](auto v){return v%3!=0;});
    assert(util::math::sum(fvs)==10);
    assert(util::math::sum(filter_inplace(vs, [](auto v){return v%3!=0;}))==10);

    size_t n_cut = 1;
    WordCounter word_count;
    auto word_counts = word_count.count(std::fstream{"../rnn++/tests/data/sentence.2.corenlp"});
    fmt::print(std::cerr, "{} words.\n", word_counts.size());
    filter_inplace(word_counts, [n_cut](auto v){return v.second>n_cut;});
    fmt::print(std::cerr, "{} words are left after filtering.\n", word_counts.size());
    for(auto elm : word_counts) assert(elm.second>n_cut);
}



void binary_find_cell_for_cdf(){
    size_t n_cut = 2;
    WordCounter word_count;
    auto word_counts = word_count.count(std::fstream{"../rnn++/tests/data/sentence.2.corenlp"});
    filter_inplace(word_counts, [n_cut](auto v){return v.second>n_cut;});

    auto counts = map(word_counts, [](auto x){return x.second;});
    for(auto x : util::zip(word_counts, counts)) assert(x.first.second==x.second);
    std::partial_sum(counts.cbegin(),counts.cend(), counts.begin());
    auto cdf = word_counts;
    for(auto elm : util::zip(cdf, word_counts)) assert(elm.first==elm.second);
    std::partial_sum(word_counts.cbegin(),word_counts.cend(), cdf.begin(),
                     [](auto x, auto y){return std::make_pair(y.first, x.second+y.second);});
    for(auto elm : util::zip(cdf, word_counts)) assert(elm.first.first==elm.second.first);
    assert(counts.back()==cdf.back().second);

//    for(auto elm : util::zip(cdf, word_counts))
//        fmt::print(std::cerr, "{} : {} vs {}\n", elm.first.first, elm.first.second, elm.second.second);
    //auto n = pdf.size();

    auto n = counts.back();
    for(decltype(n)i=0; i!=n; ++i){
        auto maybe_it = binary_find_cell(counts, i);
        auto maybe_it2 = binary_find_cell(cdf, [i](auto x){return i<x.second;});
        assert(maybe_it);
        auto it = maybe_it.value();
        assert(i<*it);
        if(it!=counts.cbegin()) assert(*(it-1)<=i);
        assert(*it==maybe_it2.value()->second);
    }
}



void weighted_sampling_benchmark(){
    size_t n_cut = 2;
//    auto word_counts = word_count(std::fstream{"../rnn++/tests/data/sentence.2.corenlp"});
    WordCounter word_count;
    auto word_counts = word_count.count(std::fstream{"../rnn++/tests/data/queries.rss.short"});
    filter_inplace(word_counts, [n_cut](auto v){return v.second>n_cut;});
    auto counts = map(word_counts, [](auto x){return x.second;});
    auto uids = map(word_counts, [](auto x){return x.first;});

    auto sum_exact=0.0;
    for(auto x : word_counts) sum_exact += 1.0*x.first.val*x.second;
    sum_exact /= util::math::sum(counts);

    auto n= 1000000;
    Timer timer{};

    std::random_device rd{};
    std::mt19937 gen{rd()};

    std::discrete_distribution<size_t> d{counts.cbegin(), counts.cend()};
    auto sum_std=0.0;
    for(int i=0; i<n; ++i) sum_std += uids[d(gen)].val;
    sum_std /= n;
    timer.here_then_reset("std::random");

    util::Sampler<WordUID,size_t> sampler{word_counts};
    std::uniform_int_distribution<size_t> uni{0, sampler.total_weight()-1};
    timer.here_then_reset("prepare custom");
    auto sum = 0.0;
    for(int i=0; i<n; ++i) sum += 1.0* sampler.sample(uni(gen)).val;
    sum /= n;
    timer.here_then_reset("finish custom");

    fmt::print(std::cerr, "{},  {} vs {}\n", sum_exact, sum_std, sum);
}

template<typename T>
bool almost_equal(T x, T y, T err=0.000001){
    return std::abs((x/y)-1) < err;
}


void negative_sampling(){
    size_t n_cut = 2;
    WordCounter word_count;
    auto word_counts = word_count.count(std::fstream{"../rnn++/tests/data/sentence.2.corenlp"});
    filter_inplace(word_counts, [n_cut](auto v){return v.second>n_cut;});
    auto neg_sampled_counts = util::map(word_counts, [](auto x){return std::make_pair(x.first, std::pow(x.second, 0.75));});

    for(auto x : zip(word_counts, neg_sampled_counts))
        assert(almost_equal(std::pow(x.first.second,0.75),x.second.second));

    auto sum_exact=0.0;
    for(auto x : word_counts) sum_exact += x.first.val*std::pow(x.second,0.75);
    auto counts = map(neg_sampled_counts, [](auto x){return x.second;});
    sum_exact /= util::math::sum(counts);

    std::random_device rd{};
    std::mt19937 gen{rd()};
    auto n=1000000;
    util::Sampler<WordUID,double> neg_sampler{neg_sampled_counts};
    std::uniform_real_distribution<double> uni{0, neg_sampler.total_weight()};
    auto sum = 0.0;
    for(int i=0; i<n; ++i) sum += 1.0* neg_sampler.sample(uni(gen)).val;
    sum /= n;

    assert(almost_equal(sum_exact,  sum, 0.005));//allow ~ 5-sigma errors.
}

void word_uid_spec(){
    fmt::print("test::word_uid_spec\n");
    WordUIDindex wordUIDs{"words.uid"};//news.en.words
    std::vector<std::string> words = {"the", "grundlegenden", u8"über", "-UNKNOWN-"};
    for(auto word : words){
        auto uid = wordUIDs[word];
        fmt::print("{} : {}\n", uid, wordUIDs[uid]);
        assert(wordUIDs[uid]==word);
    }

    std::string unknown_word{"WE60720KANFAFJ14RRaoqirh1orhaf149140"};
    auto unknown_uid = wordUIDs[unknown_word];
    assert(wordUIDs[unknown_uid]==wordrep::the_unknown_word());

    assert(wordrep::the_unknown_word_uid() == wordUIDs[wordrep::the_unknown_word()]);
    assert(wordrep::the_unknown_word() == wordUIDs[wordrep::the_unknown_word_uid()]);
}

void pos_uid_spec(){
    wordrep::POSUIDindex posUIDs{"../rnn++/tests/data/poss.uid"};
    std::vector<std::string> tokens = {"NN", "ADJ"};
    for(auto token : tokens){
        auto uid = posUIDs[token];
        fmt::print("{} : {}\n", uid, posUIDs[uid]);
        assert(posUIDs[uid]==token);
    }
    std::string unknown_token{"SADFGJOAWGFAKFJKQJRFAFWQEFASFGAG"};
    auto unknown_uid = posUIDs[unknown_token];
    assert(posUIDs[unknown_uid]=="-UNKNOWN-");
}

void voca_indexmap_spec(int argc, char** argv){
    assert(argc>1);
    auto config = util::load_json(argv[1]);

    auto voca = wordrep::VocaIndexMap::factory({util::get_str(config, "voca_bin")});
    WordUIDindex wordUIDs{"words.uid"};
    std::vector<std::string> words = {"the", "-UNKNOWN-"};
    for(auto word : words){
        auto uid = wordUIDs[word];
        auto idx = voca[uid];
        fmt::print("{} : {}.uid {}.idx\n", word, uid, idx);
        assert(voca[idx]==uid);
    }
    std::string unknown_word{"WE60720KANFAFJ14RRaoqirh1orhaf149140"};
    auto unknown_uid = wordUIDs[unknown_word];
    auto unknown_idx = voca[unknown_uid];
    assert(voca[unknown_idx] == wordUIDs[wordrep::the_unknown_word()]);
    fmt::print("{} : {}.uid {}.idx\n", unknown_word, unknown_uid, unknown_idx);


}


void test_all(int argc, char** argv){
    word_uid_spec();
    pos_uid_spec();
    voca_indexmap_spec(argc,argv);
    reverse_iterator();
    string_iterator();
    benchmark();
    hash();
    hash_composite();
    uint_to_int();
    binary_find_check();
    binary_find_benchmark();
    container_filter();
    binary_find_cell_for_cdf();
    weighted_sampling_benchmark();
    negative_sampling();
}

}//namespace test


auto serial_word_count(std::istream&& is){
    std::string line;
    WordCounter::count_type word_counts;
    while(std::getline(is, line)){
        WordCounter::WordIter text{line};
        text.iter([&word_counts](auto& word){++word_counts[word];});
    }
    return util::to_pairs(word_counts);
}



void word_prob_check(int argc, char** argv){
    assert(argc>1);
    auto config = util::load_json(argv[1]);
    auto file = util::io::h5read(util::get_str(config,"word_prob_dump"));
    auto ratio_to_score = [](auto ratio){
        auto factor = ratio+0.001;
        factor = factor<1.0? 1.0: factor;
        return 0.9*(1- 1/(factor));
    };
    auto scores = map(file.getRawData<double>({"prob.ratio"}), ratio_to_score);

    wordrep::WordImportance importance{file};
    WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};

    auto words = util::string::readlines("/home/jihuni/word2vec/ygp/words.uid");
    auto n = words.size();
    for(decltype(n)i=0; i!=n; ++i){
        auto word=words[i];
        assert(scores[i] == importance.score(wordUIDs[word]));
    }
}

void word_prob_check(){
    wordrep::WordImportance importance{"../rnn++/tests/data/word_importance",
                                       "../rnn++/tests/data/words.uid"};
    WordUIDindex wordUIDs{"../rnn++/tests/data/words.uid"};

    auto words = util::string::readlines("../rnn++/tests/data/words.uid");
    for(auto word: words){
        fmt::print("{}: {}\n", word, importance.score(wordUIDs[word]));
    }
}



int main(int argc, char** argv){
    //test::test_all(argc,argv);
    test::hash();
    test::hash_composite();
//    word_prob_check(argc,argv);
//    word_prob_check();
    return 0;

    util::Timer timer{};
    WordCounter word_count;
    auto word_counts = word_count.count(std::move(std::cin));
//    auto word_counts = serial_word_count(std::move(std::cin));
    fmt::print(std::cerr, "{} words.\n", word_counts.size());
    filter_inplace(word_counts, [](auto v){return v.second>10;});
    fmt::print(std::cerr, "{} words are left after filtering.\n", word_counts.size());
    timer.here_then_reset("Finish word count.");
    for(auto elm : word_counts)
        fmt::print(std::cout, "{} {}\n", elm.first, elm.second);
    return 0;
}

