#include "zmq.hpp"
#include "similarity/similarity.h"


int main(int /*argc*/, char** argv){
    Timer timer{};
    tbb::task_group g;

//    auto config = load_json("/data/groups/uphere/similarity_test/config.json");
    auto config = load_json(argv[1]);
    SimilaritySearch engine{config};
    std::cout << config.dump(4) << std::endl;
    timer.here_then_reset("Search engine loaded.");

    const char * protocol = "tcp://*:5555";
    zmq::context_t context (1);
    zmq::socket_t socket (context, ZMQ_REP);
    socket.bind(protocol);
    while(0){
        zmq::message_t request;
        socket.recv (&request);
        auto query = json::parse((const char*)request.data());
        std::cerr << query.dump(4) << std::endl;

        auto answer = engine.process_queries(query);
        timer.here_then_reset("Query is answered.");
        std::string aa{answer.dump(4)};
        zmq::message_t reply(aa.size());
        std::cerr<<aa.size()<<std::endl;
        std::cerr<<answer.size()<<std::endl;
        std::memcpy ((void *) reply.data (), (void*)aa.data(), aa.size());
        socket.send (reply);
    }

//    auto input = load_json("/data/groups/uphere/similarity_test/queries.json");
    auto input = load_json(argv[2]);
    auto task=[&]() {
        auto answer = engine.process_queries(input);
        timer.here_then_reset("Query is answered.");
        std::cout << answer.dump(4) << std::endl;
    };
    for(int i=0; i<1; ++i) {
        g.run(task);
        std::cerr << i << std::endl;
    }
    g.wait();
//    task();

    timer.here_then_reset("All queries are answered.");
    return 0;
}
