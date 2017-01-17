#include <fstream>
#include <fmt/printf.h>

#include "similarity/query_engine.h"

#include "data_source/corenlp_helper.h"
#include "utils/profiling.h"

int main(int argc, char** argv){
    assert(argc>2);
    auto config = util::load_json(argv[1]);
    auto queries = util::string::readlines(argv[2]);


    data::CoreNLPwebclient corenlp_client{util::get_str(config,"corenlp_client_script")};
    util::Timer timer{};
    engine::QueryEngine engine{config};
    timer.here_then_reset("Data loaded.");

    tbb::task_group g;

    auto n = queries.size();
    for(decltype(n)i=0; i!=n; ++i){
        g.run([i,&queries,&corenlp_client,&config,&engine](){
            auto query_str = queries[i];
            auto query_json = corenlp_client.from_query_content(query_str);
            query_json["query_str"] = query_str;
            auto uids = engine.register_documents(query_json);
            uids["max_clip_len"] = query_json["max_clip_len"];
            {
                std::ofstream infile{fmt::format("answers/{}.input", i)};
                infile << uids.dump(4);
            }
//            auto answer = engine.ask_query(uids);
//    auto answers = engine.ask_chain_query(uids);
//    engine.annotation_on_result(config, answers);
            auto answer = engine.ask_query_stats(uids);
            engine.annotation_on_result(config, answer["results"]);
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