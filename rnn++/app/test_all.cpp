#include "tests/test_context_rnn.h"
#include "tests/query_processing.h"

#include "similarity/config.h"
#include "utils/profiling.h"
#include "wordrep/voca_info.h"
#include "wordrep/word_uid.h"
#include "wordrep/simiarity_score.h"

int generate_similarword_table(int argc, char** argv) {
    assert(argc>1);
    auto config_json = util::load_json(argv[1]);
    engine::SubmoduleFactory factory{{config_json}};
    util::Timer timer;

    std::unique_ptr<wordrep::WordImportance> word_importance;
    std::unique_ptr<wordrep::WordUIDindex> wordUIDs;
    std::unique_ptr<wordrep::VocaInfo> voca{};

    auto load_word_uids =[&wordUIDs,&factory](){
        wordUIDs = std::make_unique<wordrep::WordUIDindex>(factory.conf.word_uid);
    };
    auto load_word_scores =[&word_importance,&factory](){
        word_importance = std::make_unique<wordrep::WordImportance>(factory.word_importance());
    };
    auto load_word_embedding = [&voca,&factory](){
        voca = std::make_unique<wordrep::VocaInfo>(factory.voca_info());
    };

    util::parallel_invoke(load_word_uids,
                          load_word_scores,
                          load_word_embedding);
    wordrep::AngleSimilarity op_sim{voca->wvecs};
    timer.here_then_reset("Load data");


    auto word = wordUIDs->get_uid("bought");
    auto wvec = voca->indexmap[word];
    auto cutoff = word_importance->score(word) * 0.7;
    for(auto elm : voca->indexmap.iter_token()){
        auto w = elm.first;
        auto vidx = elm.second;
        auto score = op_sim.score(wvec, vidx);
        if(score<cutoff) continue;
        fmt::print("{} {} {}\n", wordUIDs->str(word), wordUIDs->str(w), score);
    }
    timer.here_then_reset("Get similar words");

    for(auto& elm : word_importance->iter_token()){
        auto word = elm.first;
        auto score = elm.second;
        if(score<=0.0) continue;
        //fmt::print("{} {}\n", wordUIDs->str(word), score);
    }
    return 0;
}

int main(int argc, char** argv){
    generate_similarword_table(argc,argv);
    return 0;

    rnn::test::test_all();
    engine::test::load_query_engine_data(argc,argv);
    return 0;
}

