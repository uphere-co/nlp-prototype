#include <string>
#include <iostream>
#include <map>
#include <set>
#include <cassert>

#include <fmt/printf.h>

#include "wordrep/word_uid.h"

#include "utils/algorithm.h"
#include "utils/string.h"
#include "utils/json.h"
#include "utils/persistent_vector.h"
#include "utils/math.h"

using util::PersistentVector;
using token_t = wordrep::WordUID;
using count_t = size_t;

auto collect_count(std::istream&& is){
    std::map<std::string,count_t> counts;
    std::string line;
    while(std::getline(is, line)){
        auto elms = util::string::split(line, "\t");
        assert(elms.size()==2);
        //token_t key{std::stoll(elms[0])};
        auto key = elms[0];
        count_t count = std::stoll(elms[1]);
        counts[key] += count;
    }
    return counts;
}


namespace util{
namespace test{

void intersection_and_difference(){
    using WordUID = int64_t;
    std::vector<WordUID> xs={1,2,4,5,6,7,8,10};
    std::vector<WordUID> ys={1,3,4,6,7,8,9,11};
    std::vector<WordUID> cs={1,4,6,7,8};
    std::vector<WordUID> cs2={2,5,10};
    auto commons = intersection(xs,ys, std::equal_to<WordUID>{}, std::less<WordUID>{});
    auto xonly = not_in_intersection(xs,ys, std::equal_to<WordUID>{}, std::less<WordUID>{});
    assert(commons == cs);
    assert(commons != cs2);
    assert(xonly != cs);
    assert(xonly == cs2);
}

void test_all(){
    intersection_and_difference();
}

}//namespace wordrep::test
}//namespace wordrep

void list_new_words(util::json_t const& config){
    wordrep::WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};
    PersistentVector<token_t,token_t::val_t> voca{util::io::h5read(config["wordvec_store"]),
                                                  config["voca_name"]};

    auto counts = util::map(util::to_pairs(collect_count(std::move(std::cin))),
                       [&wordUIDs](auto x){return std::make_pair(wordUIDs[x.first],x.second);});
    //util::filter_inplace(counts, [](auto v){return v.second>9;});
    fmt::print(std::cerr, "Total {} words after filtering.\n", counts.size());
    util::sort(counts, [](auto& x, auto& y){return x.first<y.first;});

    std::vector<wordrep::WordUID> known_words = voca.get();
    std::sort(known_words.begin(), known_words.end());
    auto new_words = util::not_in_intersection(counts, known_words,
                                               [](auto c,auto w){return c.first==w;},
                                               [](auto c,auto w){return c.first <w;});

    util::sort(new_words, [](auto x, auto y){return x.second>y.second;});
    for(auto x : new_words) {
        auto uid = x.first;
        auto word = wordUIDs[uid];
        fmt::print("{:<15}\t{}\n", word, x.second);
    }
}

int main(int /*argc*/, char** argv) {
//    util::test::test_all();
//    return 0;
    auto config = util::load_json(argv[1]);
    auto dumpfile = argv[2];

    //list_new_words(config);
    wordrep::WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};
    auto counts = util::map(util::to_pairs(collect_count(std::move(std::cin))),
                            [&wordUIDs](auto x){return std::make_pair(wordUIDs[x.first],x.second);});

    auto prefix = "unigram.";
    PersistentVector<token_t,token_t::val_t> uid{"uid"};
    PersistentVector<count_t,count_t> count{"count"};

    for(auto x : counts) {
        uid.push_back(x.first);
        count.push_back(x.second);
    }
    auto file = util::io::h5rw_exist(dumpfile);
    fmt::print("Total number of words  : {}.\n", count.size());
    fmt::print("Total number of tokens : {}.\n", util::math::sum(count.get()));
    uid.write(file,   prefix);
    count.write(file, prefix);
    return 0;
}
