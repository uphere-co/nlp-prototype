#include <fmt/printf.h>

#include "similarity/word_importance.h"
#include "similarity/config.h"

#include "similarity/ygp.h"

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

//For YGP data, country info is slated in the country code column of YGP DB.
void set_zero_importance_on_country_name(util::json_t const& config){
    data::ygp::Factory factory{{config}};
    auto file = util::io::h5rw_exist(util::get_str(config, "word_prob_dump"));
    util::TypedPersistentVector<wordrep::WordUID> uids{file,"prob.word_uid"};
    util::PersistentVector<float,float> ratios{file,"prob.ratio"};

    wordrep::WordUIDindex wordUIDs = factory.common.word_uid_index();
    auto countries = factory.country_list();

    auto importances = util::zip(uids,ratios);
    for(auto country : countries){
        auto uid = wordUIDs[country];
        auto it=util::find_if(importances, [uid](auto x){return x.first==uid;});
        if(it==importances.end()) continue;
        //To make importance of country names to low, but non-zero scores,
        //set their ratio accordingly.
        it->second = wordrep::WordImportance::low_cutoff_ratio+0.1;
    }

    for(auto country : countries){
        auto uid = wordUIDs[country];
        auto it=util::find_if(importances, [uid](auto x){return x.first==uid;});
        if(it==importances.cend()) continue;
        fmt::print(std::cerr, "{} : {}\n", country, it->second);
    }

    util::TypedPersistentVector<wordrep::WordUID> uids2{"prob.word_uid",util::get_keys(importances)};
    util::PersistentVector<float,float> ratios2{"prob.ratio", util::get_values(importances)};
    uids2.overwrite(file);
    ratios2.overwrite(file);
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

int main(int argc, char** argv){
    engine::build_word_importance();
//    assert(argc>1);
//    auto config = util::load_json(argv[1]);
//    show_old_foramt_word_importance(config);
//    set_zero_importance_on_country_name(config);
    return 0;
}
