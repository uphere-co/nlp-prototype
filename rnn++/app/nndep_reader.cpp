#include <vector>
#include <algorithm>
#include <cctype>

#include "fmt/printf.h"

//#include "similarity/similarity.h"
#include "similarity/dep_similarity.h"
#include "similarity/corenlp_helper.h"

#include "wordrep/word_uid.h"
#include "wordrep/word_prob.h"
#include "wordrep/voca.h"
#include "wordrep/wordvec.h"

#include "utils/json.h"
#include "utils/hdf5.h"
#include "utils/profiling.h"
#include "utils/span.h"
#include "utils/string.h"

using namespace util::io;
using namespace wordrep;
using namespace engine;

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
    VocaIndexMap uids{load_voca("news.h5", "news.en.uids")};
    H5file prunerfile{H5name{"s2010.h5"}, hdf5::FileMode::rw_exist};
    auto raw_pruner = prunerfile.getRawData<WordUID::val_t>(H5name{"s2010.uids"});
    VocaIndexMap pruner_uids{raw_pruner};

    WordBlock_base<float,100> wvecs{load_raw_wvec("news.h5", "news.en.vecs", "float32")};
    std::vector<float> pruned_wvecs;
    std::vector<WordUID::val_t > pruned_uids;
    for(auto const& pair: uids.uid2idx) {
        auto uid = pair.first;
        if (pruner_uids.isin(uid)) {
            pruned_uids.push_back(uid.val);
            auto wvec = wvecs[uids[uid]];
            std::copy(wvec.cbegin(), wvec.cend(), std::back_inserter(pruned_wvecs));
        }
    }

    H5file outfile{H5name{"test.Google.h5"}, hdf5::FileMode::replace};
    outfile.writeRawData(H5name{"news.en.uids"}, pruned_uids);
    outfile.writeRawData(H5name{"news.en.vecs"}, pruned_wvecs);
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

void write_voca_index_col(nlohmann::json const& config){
    VocaInfo voca{config["wordvec_store"], config["voca_name"],
                  config["w2vmodel_name"], config["w2v_float_t"]};
    H5file file{H5name{config["dep_parsed_store"].get<std::string>()}, hdf5::FileMode::rw_exist};
    std::string prefix{config["dep_parsed_text"].get<std::string>()};
    using namespace util;
    {
        auto uids = deserialize<WordUID>(file.getRawData<int64_t>(H5name{prefix+".word_uid"}));
//        auto uids = deserialize<WordUID>(file.getRawData<int64_t>(H5name{prefix+".word"}));
//        file.writeRawData(H5name{prefix+".word_uid"}, serialize(uids));}
        std::vector<VocaIndex> idxs;
        for(auto uid:uids) idxs.push_back(voca.indexmap[uid]);
        file.overwriteRawData(H5name{prefix+".word"}, serialize(idxs));}
    {
        auto uids = deserialize<WordUID>(file.getRawData<int64_t>(H5name{prefix+".head_uid"}));
//        auto uids = deserialize<WordUID>(file.getRawData<int64_t>(H5name{prefix+".head_word"}));
//        file.writeRawData(H5name{prefix+".head_uid"}, serialize(uids));}
        std::vector<VocaIndex> idxs;
        for(auto uid:uids) idxs.push_back(voca.indexmap[uid]);
        file.writeRawData(H5name{prefix+".head"}, serialize(idxs));}
}


int main(int /*argc*/, char** argv){
    auto config = util::load_json(argv[1]);
//    pruning_voca();
//    convert_h5py_to_native();
//    write_WordUIDs("test.Google.h5", "news.en.words", "news.en.uids");
//    write_voca_index_col(config);
//    write_WordUIDs("s2010.h5", "s2010.words", "s2010.uids");
//    return 0;
    std::string input = argv[2];
    CoreNLPwebclient corenlp_client{config["corenlp_client_script"].get<std::string>()};
    auto query_json = corenlp_client.from_query_content(input);
//    auto query_json = corenlp_client.from_query_file(input);

    util::Timer timer{};
    DepSimilaritySearch engine{config};
    timer.here_then_reset("Data loaded.");
    auto answer = engine.process_queries(query_json);
    timer.here_then_reset("Queries are answered.");
    fmt::print("{}\n", answer.dump(4));
    return 0;
}
