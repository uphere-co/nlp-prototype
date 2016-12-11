#include "zmq.hpp"

#include "similarity/dep_similarity.h"
#include "data_source/corenlp_helper.h"
#include "data_source/rss.h"

#include "utils/profiling.h"
#include "utils/parallel.h"
#include "utils/json.h"

int main(int /*argc*/, char** argv){
    using namespace util;
    Timer timer{};

//    auto config = load_json("/data/groups/uphere/similarity_test/config.json");
    auto config = load_json(argv[1]);
    std::cerr << config.dump(4) << std::endl;

    auto dumpfile_hashes = argv[2];

//    SimilaritySearch engine{config};
//    timer.here_then_reset("SimilaritySearch engine loaded.");
//    BoWVSimilaritySearch engine{config};

    const char * protocol = "tcp://*:5555";
    zmq::context_t context (1);
    zmq::socket_t socket (context, ZMQ_REP);
    socket.bind(protocol);
    while(0) {
        zmq::message_t request;
        socket.recv(&request);
        std::string input{(const char *) request.data()};
        std::cerr << input << std::endl;
        auto input_json = nlohmann::json::parse(input);
        std::cerr << input_json.dump(4) << std::endl;
        std::string aa=input_json.dump(4);
        zmq::message_t reply(aa.size());
        std::memcpy((void *) reply.data(), (void *) aa.data(), aa.size());
        socket.send(reply);
    }
//    engine::DepSimilaritySearch engine{config};
    engine::RSSQueryEngine engine{config};
    data::CoreNLPwebclient corenlp_client{config["corenlp_client_script"].get<std::string>()};
    timer.here_then_reset("Search engine loaded.");
    while(1){
        zmq::message_t request;
        socket.recv (&request);
        std::string input{(const char*)request.data()};
        std::cerr << input << std::endl;
        auto input_json= nlohmann::json::parse(input);
        std::cerr << input_json.dump(4) << std::endl;
        if(input_json.find("raw_text")!=input_json.end()) {
            std::cerr << "Register documents"<<std::endl;
            auto query_str=input_json["raw_text"].get<std::string>();
            auto query_json = corenlp_client.from_query_content(query_str);
            query_json["query_str"] = query_str;
//        std::cerr << query_json.dump(4) << std::endl;
            auto uids = engine.register_documents(query_json);
            uids["max_clip_len"] = query_json["max_clip_len"];
//            std::cerr << uids.dump(4) << std::endl;
            std::string aa{uids.dump(4)};
            zmq::message_t reply(aa.size());
            std::memcpy((void *) reply.data(), (void *) aa.data(), aa.size());
            socket.send(reply);
        } else if (input_json.find("chain_query")!=input_json.end()){
            std::cerr << "Ask chain query"<<std::endl;
            auto answer = engine.ask_chain_query(input_json);
            data::rss::annotation_on_result(config, answer, dumpfile_hashes);
//            data::ygp::annotation_on_result(config, answer);
            //std::cerr << answer.dump(4) << std::endl;
            std::string aa{answer.dump(4)};
            zmq::message_t reply(aa.size());
            std::memcpy((void *) reply.data(), (void *) aa.data(), aa.size());
            socket.send(reply);
        } else if (input_json.find("stats_query")!=input_json.end()){
            std::cerr << "Ask stats query"<<std::endl;
            auto answer = engine.ask_query_stats(input_json);
            data::rss::annotation_on_result(config, answer["results"], dumpfile_hashes);
            std::string aa{answer.dump(4)};
            zmq::message_t reply(aa.size());
            std::memcpy((void *) reply.data(), (void *) aa.data(), aa.size());
            socket.send(reply);
        } else if (input_json.find("sents")!=input_json.end()){
            std::cerr << "Getting sentences from UIDs"<<std::endl;
            auto answer = engine.ask_sents_content(input_json);
            std::string aa{answer.dump(4)};
            zmq::message_t reply(aa.size());
            std::memcpy((void *) reply.data(), (void *) aa.data(), aa.size());
            socket.send(reply);
        } else {
            std::string aa{};
            zmq::message_t reply(aa.size());
            std::memcpy((void *) reply.data(), (void *) aa.data(), aa.size());
            socket.send(reply);
        }
    }
    return 0;
}
