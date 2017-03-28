#include <fstream>
#include <fmt/printf.h>

#include "similarity/query_engine.h"
#include "similarity/config.h"

#include "data_source/corenlp_helper.h"
#include "utils/profiling.h"


util::json_t preprocess_query(wordrep::WordCaseCorrector const& did_you_mean, util::json_t const &ask) {
    util::Timer timer;
    timer.here_then_reset(fmt::format("QueryEngine::preprocess_query is called : {}", ask.dump()));
    if (ask.find("sentences") == ask.end()) return util::json_t{};
    data::CoreNLPjson query{ask};

    std::vector<std::string> original_words;
    auto per_tokens = [&original_words](auto const &token){
        original_words.push_back(util::get_str(token,"originalText"));
    };
    query.iter_tokens(per_tokens);
    auto original_query = util::string::join(original_words, " ");
    auto corrected_words = util::map(original_words, [&did_you_mean](auto word){return did_you_mean.try_correct(word);});
    auto corrected_query = util::string::join(corrected_words, " ");

    util::json_t answer{};
    answer["received_query"]=original_query;
    answer["did_you_mean"]=corrected_query;
    answer["word_pair"] = util::json_t::array();
    for(auto pair : util::zip(original_words, corrected_words)){
        if(pair.first==pair.second) continue;
        answer["word_pair"].push_back({pair.first, pair.second});
    }
    timer.here_then_reset("QueryEngine::preprocess_query is finished.");
    return answer;
}

void stress_did_you_mean(int argc, char** argv){
    assert(argc>1);
    util::Timer timer{};
    auto config = util::load_json(argv[1]);
    engine::SubmoduleFactory factory{{config}};
    auto word_importance = factory.word_importance();
    auto did_you_mean = factory.word_case_corrector(word_importance);

    std::vector<std::string> queries;
    for(int i=0; i<1000; ++i)
        queries.push_back(fmt::format("some meaningful company in recent {} years", i));
    data::CoreNLPwebclient corenlp_client{util::get_str(config,"corenlp_client_script")};
    timer.here_then_reset("Data loaded.");

    tbb::task_group g;
    auto n = queries.size();
    for(decltype(n)i=0; i!=n; ++i){
        g.run([i,&queries,&corenlp_client,&did_you_mean](){
            auto raw_query_str = queries[i];
            auto raw_query_json = corenlp_client.from_query_content(raw_query_str);

            auto corrected_query = preprocess_query(did_you_mean, raw_query_json);
            auto query_str = corrected_query["did_you_mean"];
            auto query_json = corenlp_client.from_query_content(query_str);
            query_json["query_str"] = query_str;
            std::ofstream infile{fmt::format("answers/{}.input", i)};
            infile << query_json.dump(4);
        });
    }
    g.wait();
    timer.here_then_reset("All queries are answered.");

}
int main(int argc, char** argv){
    stress_did_you_mean(argc,argv);
    return 0;
//    assert(argc>2);
    auto config = util::load_json(argv[1]);
    //auto queries = util::string::readlines(argv[2]);
    std::vector<std::string> queries;
    for(int i=0; i<1000; ++i)
        queries.push_back(fmt::format("some meaningful company in recent {} years", i));

    data::CoreNLPwebclient corenlp_client{util::get_str(config,"corenlp_client_script")};
    util::Timer timer{};
    engine::QueryEngine engine{config};
    timer.here_then_reset("Data loaded.");

    tbb::task_group g;

    auto n = queries.size();
    for(decltype(n)i=0; i!=n; ++i){
        g.run([i,&queries,&corenlp_client,&config,&engine](){
            auto raw_query_str = queries[i];
            auto raw_query_json = corenlp_client.from_query_content(raw_query_str);

            auto corrected_query = engine.preprocess_query(raw_query_json);
            auto query_str = corrected_query["did_you_mean"];
            auto query_json = corenlp_client.from_query_content(query_str);
            query_json["query_str"] = query_str;

            auto uids = engine.register_documents(query_json);
            uids["max_clip_len"] = query_json["max_clip_len"];
            {
                std::ofstream infile{fmt::format("answers/{}.input", i)};
                infile << uids.dump(4);
            }
            auto answer = engine.ask_query_stats(uids);
            //engine.annotation_on_result(config, answer["results"]);
            {
                std::ofstream outfile{fmt::format("answers/{}.output", i)};
                outfile << answer.dump(4);
            }
        });
    }
    g.wait();
    timer.here_then_reset("All queries are answered.");

    return 0;
}
