#include <fmt/printf.h>

#include "similarity/word_importance.h"
#include "similarity/config.h"

int main(int argc, char** argv){
    assert(argc>1);
    auto config_json = util::load_json(argv[1]);
    engine::SubmoduleFactory factory{{config_json}};

    auto wordUIDs = factory.word_uid_index();
    auto word_importance = factory.word_importance();

    for(auto elm : word_importance.all_scores()){
        if(elm.second==0.0) continue;
        fmt::print("{} {}\n", wordUIDs[elm.first], elm.second);
    }
    return 0;
}
