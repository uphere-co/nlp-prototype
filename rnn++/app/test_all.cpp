#include "tests/test_context_rnn.h"
#include "tests/query_processing.h"

#include "similarity/config.h"
#include "utils/profiling.h"
#include "utils/flatbuffers/io.h"
#include "wordrep/voca_info.h"
#include "wordrep/word_uid.h"
#include "wordrep/simiarity_score.h"
#include "wordrep/io.h"

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

    auto wvecs = voca->wvecs.copy();
    auto vec_dim = wvecs.dim;
    for(auto vidx : wvecs.iter_row_idx() ){
        auto norm_factor = 1.0/util::math::norm_L2(wvecs[vidx]);
        for(decltype(vec_dim)j=0; j!=vec_dim; ++j) wvecs.at(vidx,j) *= norm_factor;
    }
    timer.here_then_reset("L1 normalize word vectors.");

    tbb::concurrent_vector<wordrep::io::SimilarWordPair> similar_words;
    similar_words.reserve(voca->indexmap.size()*100);
    auto word_scores = util::to_pairs(word_importance->all_scores());
    auto n = word_scores.size();
    tbb::parallel_for(decltype(n){0}, n, [&word_scores,&word_importance,&voca,&wvecs,&similar_words](auto i){
        auto& elm = word_scores[i];
        auto word = elm.first;
        if(word_importance->is_noisy_word(word)) return;

        auto word_vidx = voca->indexmap[word];
        auto cutoff = word_importance->score(word) * 0.7;
        for(auto elm : voca->indexmap.iter_token()){
            auto w = elm.first;
            auto vidx = elm.second;
            auto similarity = util::math::dot(wvecs[word_vidx], wvecs[vidx]);
            if(similarity<cutoff) continue;
            similar_words.push_back({word.val, w.val, similarity});
        }
    });
    timer.here_then_reset("Get similar words");

    tbb::parallel_sort(similar_words.begin(), similar_words.end());
    timer.here_then_reset("Sort similar words");

    auto tokens = util::to_vector(similar_words);
    flatbuffers::FlatBufferBuilder builder;
    auto tokens_serialized = builder.CreateVectorOfStructs(tokens);
    auto entities = wordrep::io::CreateSimilarWords(builder, tokens_serialized);
    builder.Finish(entities);
    util::io::fb::to_file(builder, "similar_words.bin");
    timer.here_then_reset("Write to file");

    return 0;
}

void load_similarword_table(int argc, char** argv) {
    assert(argc>1);
    auto config_json = util::load_json(argv[1]);
    engine::SubmoduleFactory factory{{config_json}};
    util::Timer timer;
    wordrep::WordUIDindex wordUIDs{factory.conf.word_uid};
    timer.here_then_reset("Load data");

    auto data = util::io::fb::load_binary_file("similar_words.bin");
    auto rbuf = wordrep::io::GetSimilarWords(data.get());
    tbb::concurrent_vector<wordrep::io::SimilarWordPair> similar_words;
    similar_words.reserve(rbuf->pairs()->size());
    for(auto v : *rbuf->pairs())
        similar_words.push_back(*v);
    timer.here_then_reset("Load SimilarWordPair from binary files");
    for(auto elm : similar_words){
        fmt::print("{} {} {}\n", wordUIDs.str(elm.word()), wordUIDs.str(elm.sim()), elm.similarity());
    }
}

int main(int argc, char** argv){
//    generate_similarword_table(argc,argv);
    load_similarword_table(argc,argv);
    return 0;

    rnn::test::test_all();
    engine::test::load_query_engine_data(argc,argv);
    return 0;
}

