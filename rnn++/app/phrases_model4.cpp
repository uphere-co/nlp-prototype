#include "parser/parser.h"
#include "parser/wordvec.h"
#include "utils/binary_tree.h"
#include "utils/parallel.h"

#include "tests/test_voca_wordvec.h"

using namespace rnn::config;
using namespace rnn::simple_model;
using namespace rnn::simple_model::detail;
using namespace rnn::wordrep;

int main(int /*argc*/, char** argv){
    // using namespace test;
    // test_collecting_new_voca();
    // return 0;

    auto param = load_param("rnn_params.h5", argv[1], util::DataType::dp);
    VocaInfo rnn{file_name, voca_name, w2vmodel_name, w2vmodel_f_type};
    auto dataset=TokenizedSentences{argv[2]};

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
    write_to_disk(phrases, argv[2]);
    Voca new_voca{phrases.serialize_words()};
    print_words(new_voca);
    return 0;
}
