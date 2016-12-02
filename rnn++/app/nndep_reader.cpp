#include <fmt/printf.h>

#include "similarity/dep_similarity.h"
#include "data_source/ygp_db.h"
#include "data_source/rss.h"
#include "data_source/corenlp_helper.h"

#include "utils/profiling.h"
#include "utils/string.h"


namespace wordrep {

struct DependencyNode {
    DPTokenIndex idx;
    DependencyNode *governor;
    std::vector<DependencyNode *> dependents;
};

}//namespace wordrep;

namespace wordrep{
namespace test{

void dependency_graph(){
    data::CoreNLPjson test_input{std::string{"../rnn++/tests/data/sentence.1.corenlp"}  };
    data::CoreNLPjson test_input2{std::string{"../rnn++/tests/data/sentence.2.corenlp"}  };
    WordUIDindex wordUIDs{"../rnn++/tests/data/words.uid"};
    POSUIDindex const posUIDs{"../rnn++/tests/data/poss.uid"};
    ArcLabelUIDindex const arclabelUIDs{"../rnn++/tests/data/dep.uid"};

    DepParsedTokens tokens{};
    tokens.append_corenlp_output(wordUIDs, posUIDs, arclabelUIDs, test_input);
    tokens.append_corenlp_output(wordUIDs, posUIDs, arclabelUIDs, test_input2);
    tokens.build_sent_uid(0);
    //tokens.build_voca_index(voca.indexmap);

    auto sents = tokens.IndexSentences();
    fmt::print(std::cerr, "{} {}\n", tokens.n_tokens(), sents.size());
    for(auto sent : sents){
        std::vector<DependencyNode> nodes(sent.size());
        for (auto idx = sent.beg; idx != sent.end; ++idx) {
            auto &node = nodes[tokens.word_pos(idx).val];
            auto head_pos = tokens.head_pos(idx).val;
            if(head_pos<0) continue;
            auto &head = nodes[head_pos];
            node.idx = idx;
            node.governor = &head;
            head.dependents.push_back(&node);
        }
        for(auto const &node : nodes){
            auto uid = tokens.word_uid(node.idx);
            fmt::print(std::cerr, "{:<15} ", wordUIDs[uid]);
            if(node.governor) fmt::print(std::cerr, "head :{:<15} ", wordUIDs[tokens.word_uid(node.governor->idx)]);
            else fmt::print(std::cerr, "head :{:<15} ", "");
            fmt::print(std::cerr, "child:");
            for(auto child : node.dependents) fmt::print(std::cerr, "{:<15} ", wordUIDs[tokens.word_uid(child->idx)]);
            std::cerr<<std::endl;
        }
        fmt::print(std::cerr, ": {}\n", sent.size());
    }
}

}//namespace wordrep::test
}//namespace wordrep

using namespace wordrep;
using namespace engine;

int main(int /*argc*/, char** argv){
    auto config = util::load_json(argv[1]);
    std::string input = argv[2];
    //auto dumpfile_hashes = argv[3];
    wordrep::test::dependency_graph();
    return 0;

    data::CoreNLPwebclient corenlp_client{config["corenlp_client_script"].get<std::string>()};
    auto query_str = util::string::read_whole(input);
    auto query_json = corenlp_client.from_query_content(query_str);
    query_json["query_str"] = query_str;

    util::Timer timer{};

    DepSimilaritySearch engine{config};
    //RSSQueryEngine engine{config};
    timer.here_then_reset("Data loaded.");
    auto uids = engine.register_documents(query_json);
    uids["max_clip_len"] = query_json["max_clip_len"];
    //fmt::print("{}\n", uids.dump(4));
    timer.here_then_reset("Registered documents.");
    auto answers = engine.ask_query(uids);
    timer.here_then_reset("Processed a query.");
    data::ygp::annotation_on_result(config, answers);
    timer.here_then_reset("Query output annotation.");
    fmt::print("{}\n", answers.dump(4));
    fmt::print("\n\n--------- ------------\nA chain query find results:\n", answers.dump(4));
    timer.here_then_reset("Begin a chain query.");
    auto chain_answers = engine.ask_chain_query(uids);
    timer.here_then_reset("Processed a chain query.");
    //data::rss::annotation_on_result(config, chain_answers, dumpfile_hashes);
    data::ygp::annotation_on_result(config, chain_answers);
    timer.here_then_reset("A chain query output annotatoin.");
    fmt::print("{}\n", chain_answers.dump(4));
    timer.here_then_reset("Queries are answered.");

    return 0;
}
