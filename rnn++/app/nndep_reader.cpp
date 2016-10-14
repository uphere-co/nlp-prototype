#include <vector>
#include <algorithm>
#include <cctype>
#include "fmt/printf.h"

#include "parser/voca.h"
#include "parser/parser.h"
#include "similarity/similarity.h"

#include "wordrep/word_uid.h"
#include "wordrep/word_prob.h"
#include "wordrep/voca.h"

#include "utils/json.h"
#include "utils/hdf5.h"
#include "utils/profiling.h"
#include "utils/span.h"
#include "utils/string.h"

using namespace util::io;
using namespace wordrep;

void write_WordUIDs(std::string filename, std::string voca_name, std::string uids_name){
    H5file file{H5name{filename}, hdf5::FileMode::rw_exist};
    auto raw = file.getRawData<char>(H5name{voca_name});
    auto words = util::string::unpack_word_views(raw);
    WordUIDindex wordUIDs{"/home/jihuni/word2vec/ygp/words.uid"};

    std::vector<WordUID::val_t> uids;
    for(auto word : words) uids.push_back(wordUIDs[word].val);
    file.writeRawData(H5name{uids_name}, uids);
}
void pruning_voca(){
    H5file file{H5name{"news.h5"}, hdf5::FileMode::rw_exist};
    auto raw = file.getRawData<WordUID::val_t>(H5name{"news.en.uids"});
    VocaIndexMap uids{raw};
    H5file prunerfile{H5name{"s2010.h5"}, hdf5::FileMode::rw_exist};
    auto raw_pruner = prunerfile.getRawData<WordUID::val_t>(H5name{"s2010.uids"});
    VocaIndexMap pruner_uids{raw_pruner};

    std::vector<WordUID::val_t > pruned_uids;
    std::vector<VocaIndex::val_t > pruned_idxs;
    for(auto const& pair: uids.uid2idx) {
        if (pruner_uids.isin(pair.first)) {
            pruned_uids.push_back(pair.first.val);
            pruned_idxs.push_back(pair.second.val);
        }
    }

//    auto n = s2010.voca.size();
//    std::vector<std::string> words;
//    std::vector<float> vec_raw;
//    words.push_back(s2010.voca[0]);
//    auto wvec = s2010.voca_vecs[0];
//    std::copy(wvec.cbegin(), wvec.cend(), std::back_inserter(vec_raw));
//    for(decltype(n)i=0; i!=n; ++i){
//        auto word = s2010.voca[i];
//        auto idx = rnn.word2idx.getIndex(rnn::wordrep::Word{word});
//        if(idx==0) continue;
////        fmt::print("{}\n", word);
//        words.push_back(word);
//        auto wvec = rnn.voca_vecs[idx];
//        if (rnn.voca[idx]!=word) fmt::print("{} should be {}\n", rnn.voca[idx], word);
//        std::copy(wvec.cbegin(), wvec.cend(), std::back_inserter(vec_raw));
//    }
//    auto word_raw = util::string::pack_words(words);
//
    H5file outfile{H5name{"test.Google.h5"}, hdf5::FileMode::replace};
    outfile.writeRawData(H5name{"news.en.uids"}, pruned_uids);
//    outfile.writeRawData(H5name{"news.en.vecs"}, vec_raw);
//    outfile.writeRawData(H5name{"news.en.words"}, word_raw);
}

void print_CoreNLP_output(nlohmann::json const &json){
    for (auto it = json.begin(); it != json.end(); ++it) {
        std::cout << it.key() << "\n";
    }
    WordUIDindex wordUIDs{"/home/jihuni/word2vec/ygp/words.uid"};
    WordImportance word_cutoff{H5file{H5name{"/home/jihuni/word2vec/ygp/prob.test.h5"}, hdf5::FileMode::read_exist}};
    for(auto const& sent_json : json["sentences"] ){
        for(auto const &token : sent_json["tokens"]){
            auto word_pidx = token["index"].get<int64_t>()-1;
            auto word = token["word"].get<std::string>();
            auto pos = token["pos"].get<std::string>();
            fmt::print("{:<10}\t{}\t{}\t{}\n", word, word_pidx, word_cutoff.ratio(wordUIDs[word]), pos);
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
    pruning_voca();
//    convert_h5py_to_native();
//    write_WordUIDs("news.h5", "news.en.words", "news.en.uids");
//    write_WordUIDs("s2010.h5", "s2010.words", "s2010.uids");
    return 0;
    auto config = util::load_json(argv[1]);
    auto query_json = util::load_json(argv[2]);
    auto output_json = util::load_json(argv[3]);

//    print_CoreNLP_output(output_json);
    WordUIDindex wordUIDs{"/home/jihuni/word2vec/ygp/words.uid"};
    WordImportance word_cutoff{H5file{H5name{"/home/jihuni/word2vec/ygp/prob.test.h5"}, hdf5::FileMode::read_exist}};
    for(auto const& sent_json : output_json["sentences"] ){
        for(auto const &token : sent_json["tokens"]){
            auto word_pidx = token["index"].get<int64_t>()-1;
            auto word = token["word"].get<std::string>();
            auto pos = token["pos"].get<std::string>();
            auto word_uid = wordUIDs[word];
            fmt::print("{:<10}\t{:<10}\t{}\t{}\t{}\n",
                       word, wordUIDs[word_uid], word_pidx, word_cutoff.cutoff(word_uid), pos);
        }
        fmt::print("----------------------------------------\n");
    }
    return 0;
    util::Timer timer{};
    DepParseSearch engine{config};
    timer.here_then_reset("Data loaded.");
    auto answer = engine.process_queries(query_json);
    timer.here_then_reset("Queries are answered.");
    fmt::print("{}\n", answer.dump(4));
    return 0;
}
