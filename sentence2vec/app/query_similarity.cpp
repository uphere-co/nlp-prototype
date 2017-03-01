#include <fmt/printf.h>
#include <zmq.hpp>

#include "similarity/query_engine.h"
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

    auto port = argv[2];

//    SimilaritySearch engine{config};
//    timer.here_then_reset("SimilaritySearch engine loaded.");
//    BoWVSimilaritySearch engine{config};

    std::string protocol = fmt::format("tcp://*:{}", port);
    fmt::print(std::cerr, "Listen to {}\n", protocol);
    zmq::context_t context (1);
    zmq::socket_t socket (context, ZMQ_REP);
    socket.bind(protocol.c_str());
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
//    engine::YGPQueryEngine engine{config};
//    using data::ygp::annotation_on_result;
    engine::QueryEngine engine{config};
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
        } else if (input_json.find("ask_query")!=input_json.end()){
            std::cerr << "Ask  query"<<std::endl;
            auto answer = engine.ask_query(input_json);
            engine.annotation_on_result(config, answer);
            //std::cerr << answer.dump(4) << std::endl;
            std::string aa{answer.dump(4)};
            zmq::message_t reply(aa.size());
            std::memcpy((void *) reply.data(), (void *) aa.data(), aa.size());
            socket.send(reply);
        } else if (input_json.find("chain_query")!=input_json.end()){
            std::cerr << "Ask chain query"<<std::endl;
            auto answer = engine.ask_chain_query(input_json);
            engine.annotation_on_result(config, answer);
            //std::cerr << answer.dump(4) << std::endl;
            std::string aa{answer.dump(4)};
            zmq::message_t reply(aa.size());
            std::memcpy((void *) reply.data(), (void *) aa.data(), aa.size());
            socket.send(reply);
        } else if (input_json.find("stats_query")!=input_json.end()){
            std::cerr << "Ask stats query"<<std::endl;
            auto answer = engine.ask_query_stats(input_json);
            std::cerr << "Got stats query"<<std::endl;
            engine.annotation_on_result(config, answer["results"]);
            std::string aa{answer.dump(4)};
            zmq::message_t reply(aa.size());
            std::memcpy((void *) reply.data(), (void *) aa.data(), aa.size());
            socket.send(reply);
        } else if (input_json.find("query_suggestion")!=input_json.end()){
            std::cerr << "Ask query suggestion"<<std::endl;
            auto answer = engine.ask_query_suggestion(input_json);
            std::cerr << "Got query suggestions"<<std::endl;
            std::cerr << answer.dump(4) << std::endl;
            std::string aa{answer.dump(4)};
            zmq::message_t reply(aa.size());
            std::memcpy((void *) reply.data(), (void *) aa.data(), aa.size());
            socket.send(reply);
        } else if (input_json.find("sents")!=input_json.end()){
            std::cerr << "Getting sentences from UIDs"<<std::endl;
            auto answer = engine.ask_sents_content(input_json);
            std::cerr << answer.dump(4) << std::endl;
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
