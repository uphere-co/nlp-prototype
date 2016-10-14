#include <vector>
#include <algorithm>
#include <cctype>
#include "fmt/printf.h"

#include "parser/voca.h"
#include "parser/parser.h"
#include "similarity/similarity.h"

#include "utils/json.h"
#include "utils/hdf5.h"
#include "utils/profiling.h"
#include "utils/span.h"
#include "utils/string.h"

using namespace util::io;

void pruning_voca(){
    rnn::simple_model::VocaInfo rnn{"news.h5", "news.en.words", "news.en.vecs",
                                    util::datatype_from_string("float32")};
    rnn::simple_model::VocaInfo s2010{"s2010.h5", "s2010.words", "s2010.vecs",
                                      util::datatype_from_string("float32")};
    auto n = s2010.voca.size();
    std::vector<std::string> words;
    std::vector<float> vec_raw;
    words.push_back(s2010.voca[0]);
    auto wvec = s2010.voca_vecs[0];
    std::copy(wvec.cbegin(), wvec.cend(), std::back_inserter(vec_raw));
    for(decltype(n)i=0; i!=n; ++i){
        auto word = s2010.voca[i];
        auto idx = rnn.word2idx.getIndex(rnn::wordrep::Word{word});
        if(idx==0) continue;
//        fmt::print("{}\n", word);
        words.push_back(word);
        auto wvec = rnn.voca_vecs[idx];
        if (rnn.voca[idx]!=word) fmt::print("{} should be {}\n", rnn.voca[idx], word);
        std::copy(wvec.cbegin(), wvec.cend(), std::back_inserter(vec_raw));
    }
    auto word_raw = util::string::pack_words(words);

    H5file outfile{H5name{"test.Google.h5"}, hdf5::FileMode::replace};
    outfile.writeRawData(H5name{"news.en.vecs"}, vec_raw);
    outfile.writeRawData(H5name{"news.en.words"}, word_raw);
}

void print_CoreNLP_output(nlohmann::json const &json){
    for (auto it = json.begin(); it != json.end(); ++it) {
        std::cout << it.key() << "\n";
    }
    //nlohmann::json& sent_json = query_json['sentences'][0];
//    fmt::print("{}\n", sent_json.dump());
    for(auto const& sent_json : json["sentences"] ){
        for(auto const &token : sent_json["tokens"]){
            auto word_pidx = token["index"].get<int64_t>()-1;
            fmt::print("{} {} {}\n", word_pidx, token["word"].get<std::string>(), token["pos"].get<std::string>());
        }
        for(auto const &x : sent_json["basic-dependencies"]){
//            word[i] = word2idx.getIndex(rnn::wordrep::Word{x["dependentGloss"].get<std::string>()});
            auto word_pidx = x["dependent"].get<int64_t>()-1;
//            head_word[i] = word2idx.getIndex(rnn::wordrep::Word{x["governorGloss"].get<std::string>()});
            auto head_pidx = x["governor"].get<int64_t>()-1;
//            arc_label[i]= x["dep"];
            fmt::print("{} {}\n", word_pidx, head_pidx);
        }
        fmt::print("----------------------------------------\n");
    }
}
int main(int /*argc*/, char** argv){
//    pruning_voca();
//    convert_h5py_to_native();
//    return 0;
    auto config = util::load_json(argv[1]);
    auto query_json = util::load_json(argv[2]);
    auto output_json = util::load_json(argv[3]);
    print_CoreNLP_output(output_json);

    return 0;
    util::Timer timer{};
    DepParseSearch engine{config};
    timer.here_then_reset("Data loaded.");
    auto answer = engine.process_queries(query_json);
    timer.here_then_reset("Queries are answered.");
    fmt::print("{}\n", answer.dump(4));
    return 0;
}
