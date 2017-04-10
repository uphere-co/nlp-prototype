#include <fmt/printf.h>

#include "similarity/word_importance.h"
#include "similarity/config.h"

namespace {
std::vector<float> to_float(std::vector<double> const &vec) {
    std::vector<float> floats;
    for (auto v : vec) floats.push_back(v);
    return floats;
}
}//nameless namespace

void from_hdf5_to_fb(std::string hdf5_filename,
                     std::string output_filename){
    wordrep::WordImportance word_importance{util::io::h5read(hdf5_filename)};
    auto uids   = util::get_keys(word_importance.all_scores());
    auto scores = util::get_values(word_importance.all_scores());
    util::io::fb::to_file(util::serialize(uids),   {output_filename+".uids.bin"});
    util::io::fb::to_file(to_float(scores), {output_filename+".scores.bin"});
}
int main(int argc, char** argv){
    from_hdf5_to_fb("/opt/testset.rss/prob.h5", "word.score");
    return 0;
    assert(argc>1);
    auto config_json = util::load_json(argv[1]);
    engine::SubmoduleFactory factory{{config_json}};
    auto wordUIDs = factory.word_uid_index();

    wordrep::WordImportanceFile score_file{{"word.score.uids.bin"}, {"word.score.scores.bin"}};
    auto word_importance = wordrep::WordImportance::factory(score_file);


    for(auto elm : word_importance.all_scores()){
        if(elm.second==0.0) continue;
        fmt::print("{} {}\n", wordUIDs[elm.first], elm.second);
    }
    return 0;
}
