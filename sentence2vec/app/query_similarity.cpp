#include "zmq.hpp"

#include "similarity/dep_similarity.h"
#include "similarity/corenlp_helper.h"

#include "utils/profiling.h"
#include "utils/parallel.h"

int main(int /*argc*/, char** argv){
    using namespace engine;
    using namespace util;
    Timer timer{};

//    auto config = load_json("/data/groups/uphere/similarity_test/config.json");
    auto config = load_json(argv[1]);
    std::cerr << config.dump(4) << std::endl;

//    SimilaritySearch engine{config};
//    timer.here_then_reset("SimilaritySearch engine loaded.");
//    BoWVSimilaritySearch engine{config};
    DepSimilaritySearch engine{config};
    CoreNLPwebclient corenlp_client{config["corenlp_client_script"].get<std::string>()};
    timer.here_then_reset("Search engine loaded.");

    const char * protocol = "tcp://*:5555";
    zmq::context_t context (1);
    zmq::socket_t socket (context, ZMQ_REP);
    socket.bind(protocol);
    while(1){
        zmq::message_t request;
        socket.recv (&request);

        std::string input{(const char*)request.data()};
        auto query_json = corenlp_client.from_query_content(input);
//        std::cerr << query_json.dump(4) << std::endl;
        auto uids = engine.register_documents(query_json);
        uids["max_clip_len"] = query_json["max_clip_len"];
        std::cerr<<uids.dump(4)<<std::endl;
        auto answer = engine.process_query(uids);
//        std::cerr<<answer.dump(4)<<std::endl;
        std::string aa{answer.dump(4)};
        zmq::message_t reply(aa.size());
        std::cerr<<aa.size()<<std::endl;
        std::cerr<<answer.size()<<std::endl;
        std::memcpy ((void *) reply.data (), (void*)aa.data(), aa.size());
        socket.send (reply);
    }
    return 0;
}
