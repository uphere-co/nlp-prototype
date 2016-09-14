#include "parser/parser.h"
#include "parser/wordvec.h"
#include "utils/binary_tree.h"
#include "utils/parallel.h"
#include "utils/json.h"

#include "tests/test_voca_wordvec.h"

using namespace rnn::config;
using namespace rnn::simple_model;
using namespace rnn::simple_model::detail;
using namespace rnn::wordrep;


int main(int /*argc*/, char** argv){
    // using namespace test;
    // test_collecting_new_voca();
    // return 0;

    auto config = util::load_json(argv[1]);

    auto param = load_param(config["rnn_param_store"], config["rnn_param_uid"], util::DataType::dp);
    VocaInfo rnn{config["wordvec_store"], config["voca_name"], config["w2vmodel_name"], util::DataType::dp};
    std::string input_name = config["phrase_rawdata"];
    auto dataset=TokenizedSentences{input_name};

    auto &lines = dataset.val;
    auto get_phrases = [&](auto const &sent) {
        auto init_nodes = rnn.initialize_tree(sent);
        DPtable table=dp_merging(param, init_nodes);
        auto phrase_nodes = table.get_phrases();

        WordVectors phrases{};
        for(auto const &node : phrase_nodes){
            phrases.add_word(Word{node->name.val}, node->vec);
        }
        return phrases;
    };
    auto phrases = util::parallel_reducer(lines.cbegin(), lines.cend(), get_phrases, WordVectors{});
    write_to_disk(phrases, config["phrase_store"], config["phrase_name_prefix"]);
    Voca new_voca{phrases.serialize_words()};
    print_words(new_voca);
    return 0;
}
