#include <fmt/printf.h>

#include "similarity/word_importance.h"
#include "similarity/config.h"

#include "utils/json.h"
#include "utils/persistent_vector.h"
#include "utils/versioned_name.h"
#include "utils/algorithm.h"

void show_old_foramt_word_importance(util::json_t const& config){
    engine::SubmoduleFactory factory{{config}};
    auto file = util::io::h5read(util::get_str(config, "word_prob_dump"));
    util::TypedPersistentVector<wordrep::WordUID> uids{file,"prob.word_uid"};
    //util::PersistentVector<double,double> ratios{file,"prob.ratio"};
    auto ratios = file.getRawData<double>({"prob.ratio"});
    auto p_main = file.getRawData<double>({"prob.main"});
    auto p_summary = file.getRawData<double>({"prob.summary"});
    auto p_both = file.getRawData<double>({"prob.both"});

    wordrep::WordUIDindex wordUIDs = factory.word_uid_index();
    auto ratio_per_uids = util::zip(uids.get(),ratios);
    util::sort(ratio_per_uids, [](auto x, auto y){return x.second>y.second;});
    auto n= ratios.size();
    for(decltype(n)i=0; i!=n; ++i){
        fmt::print("{} : {} {} {} {}\n", wordUIDs[uids[i]], ratios[i], p_main[i], p_summary[i],p_both[i]);
    }
}

namespace wordrep{
namespace test{

void unknown_word_importance(engine::SubmoduleFactory const& factory){
    WordUIDindex wordUIDs = factory.word_uid_index();
    VocaInfo voca         = factory.voca_info();
    WordImportance importance = factory.word_importance();

    assert(importance.score(wordrep::the_unknown_word_uid())==0.0);
}


void test_all(int argc, char** argv){
    assert(argc>1);
    auto config_json = util::load_json(argv[1]);
    engine::Config config{config_json};
    engine::SubmoduleFactory factory{config};
    unknown_word_importance(factory);
}
}//namespace wordrep::test
}//namespace wordrep

int main(){
    engine::build_word_importance();
//    show_old_foramt_word_importance(config);
    return 0;
}
