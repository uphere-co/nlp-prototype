#include <vector>
#include <algorithm>
#include <cctype>

#include "fmt/printf.h"
#include "csv/csv.h"

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
#include "utils/parallel.h"
#include "utils/base_types.h"

#include <fstream>
#include "utils/random.h"

using namespace util::io;
using namespace wordrep;
using namespace engine;

void write_WordUIDs(std::string uid_dump, std::string filename, std::string voca_name, std::string uids_name){
    H5file file{H5name{filename}, hdf5::FileMode::rw_exist};
    auto raw = file.getRawData<char>(H5name{voca_name});
    auto words = util::string::unpack_word_views(raw);
    WordUIDindex wordUIDs{uid_dump};

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

void write_voca_index_col(VocaInfo const &voca, std::string filename, std::string prefix){
    H5file file{H5name{filename}, hdf5::FileMode::rw_exist};
    using namespace util;
    {
        auto uids = deserialize<WordUID>(file.getRawData<int64_t>(H5name{prefix+".word_uid"}));
        std::vector<VocaIndex> idxs;
        for(auto uid:uids) idxs.push_back(voca.indexmap[uid]);
        file.writeRawData(H5name{prefix+".word"}, serialize(idxs));
    }
    {
        auto uids = deserialize<WordUID>(file.getRawData<int64_t>(H5name{prefix+".head_uid"}));
        std::vector<VocaIndex> idxs;
        for(auto uid:uids) idxs.push_back(voca.indexmap[uid]);
        file.writeRawData(H5name{prefix+".head"}, serialize(idxs));
    }
}

void generate_sent_uid(std::string filename, std::string prefix){
    H5file file{H5name{filename}, hdf5::FileMode::rw_exist};
    auto sent_idx = util::deserialize<SentIndex>(file.getRawData<int64_t>(H5name{prefix+".sent_idx"}));
    std::vector<SentUID> sent_uid;
    auto beg=sent_idx.cbegin();
    auto end=sent_idx.cend();
    auto it=beg;
    SentIndex current_idx{*it};
    SentUID current_uid{};
    while(it!=end) {
        if( *it == current_idx) {sent_uid.push_back(current_uid);}
        else {
            current_idx=*it;
            sent_uid.push_back(++current_uid);
        }
        ++it;
    }
    file.writeRawData(H5name{prefix+".sent_uid"}, util::serialize(sent_uid));
}

void write_column(std::vector<int64_t> rows, std::string filename,
                  std::string prefix, std::string colname){
    H5file file{H5name{filename}, hdf5::FileMode::rw_exist};
    file.writeRawData(H5name{prefix+colname}, rows);
}

void indexing_csv(const char* file){
//    io::CSVReader<1, io::trim_chars<' ', '\t'>, io::no_quote_escape<','>> in(file);
    io::CSVReader<1, io::trim_chars<' ', '\t'>, io::double_quote_escape<',', '"'>> in(file);
    in.read_header(io::ignore_extra_column, "row_str");
    std::string row_str;
    CoreNLPwebclient corenlp_client{"../rnn++/scripts/corenlp.py"};

    std::vector<std::string> rows;
    while(in.read_row(row_str)) rows.push_back(row_str);
    auto n = rows.size();
    tbb::parallel_for(tbb::blocked_range<decltype(n)>{0,100}, [&](tbb::blocked_range<decltype(n)> const &r){
        for(auto i=r.begin(); i!=r.end(); ++i){
            auto row_str = rows[i];
            if(row_str.size()<10) return;
            auto query_json = corenlp_client.from_query_content(row_str);
        }
    });
}

void QueryAndDumpCoreNLPoutput(const char* file){
    CoreNLPwebclient corenlp_client{"../rnn++/scripts/corenlp.py"};
//    io::CSVReader<3, io::trim_chars<' ', '\t'>, io::double_quote_escape<',', '"'>> in(file);
////    in.read_header(io::ignore_extra_column, "row_str");
//    int64_t col_uid, row_idx;
//    std::string row_str;
//    std::vector<std::string> rows;
//    while(in.read_row(col_uid, row_idx, row_str)) {
//        rows.push_back(row_str);
//        if (row_str.size() < 6) assert(0);
//    }

    std::vector<std::string> rows = util::string::readlines(file);

    auto n = rows.size();
    tbb::parallel_for(tbb::blocked_range < decltype(n) > {0, n},
                      [&](tbb::blocked_range<decltype(n)> const &r) {
                          for (auto i = r.begin(); i != r.end(); ++i) {
                              auto const &row_str = rows[i];
                              auto const &parsed_json = corenlp_client.from_query_content(row_str);
                              std::ofstream temp_file;
                              temp_file.open(fmt::format("/data/jihuni/corenlp/news.{:010}", i));
                              temp_file << parsed_json.dump(4);
                              temp_file.close();
                          }
                      });
    //    using jsonvec_t = std::vector<std::pair<int64_t, nlohmann::json>>;
//    auto outputs=tbb::parallel_reduce(
//            tbb::blocked_range<decltype(n)>{0,n},
//            jsonvec_t{},
//            //current_sum should be const & or copied by value.
//            [&]( tbb::blocked_range<decltype(n)> const &r, jsonvec_t current_sum ) {
//                for(auto i=r.begin(); i!=r.end(); ++i) {
//                    auto const &row_str = rows[i];
//                    if (row_str.size() < 6) continue;
//                    auto const &parsed_json = corenlp_client.from_query_content(row_str);
//                    current_sum.push_back(std::make_pair(i,parsed_json));
//                }
//                return current_sum;
//            },
//            [](jsonvec_t const &x,jsonvec_t const &y){
//                jsonvec_t sum{x};
//                std::copy(y.cbegin(), y.cend(), std::back_inserter(sum));
//                return sum;
//            }
//    );
    return;
}
void ParseWithCoreNLP(nlohmann::json const &config, const char* file) {
    VocaInfo voca{config["wordvec_store"], config["voca_name"],
                  config["w2vmodel_name"], config["w2v_float_t"]};
    WordUIDindex wordUIDs{config["word_uids_dump"].get<std::string>()};
    POSUIDindex posUIDs{config["pos_uids_dump"].get<std::string>()};
    ArcLabelUIDindex arclabelUIDs{config["arclabel_uids_dump"].get<std::string>()};

    auto filename = "ygp.h5";
    auto prefix = "ygp";

    std::vector<std::string> dumps = util::string::readlines(file);
    DepParsedTokens tokens{};
    for(auto const &filename : dumps){
        auto parsed_json = util::load_json(filename);
        if(parsed_json.size()==0) continue;
        fmt::print("{}\n", filename);
        tokens.append_corenlp_output(wordUIDs, posUIDs, arclabelUIDs, parsed_json);
    }
    tokens.write_to_disk(filename, prefix);
}

void GenerateExtraIndexes(nlohmann::json const &config, const char* file, const char* raw_csv) {
    VocaInfo voca{config["wordvec_store"], config["voca_name"],
                  config["w2vmodel_name"], config["w2v_float_t"]};

    std::vector<std::string> outputs = util::string::readlines(file);
    std::vector<std::string> dumped_outputs = util::string::readlines(fmt::format("{}.dumped", file));
    io::CSVReader<3, io::trim_chars<' ', '\t'>, io::double_quote_escape<',', '"'>> in(raw_csv);
//    in.read_header(io::ignore_extra_column, "row_str");
    int64_t i_col, i_row;
    std::string raw_str;
    std::vector<int64_t> i_cols, i_rows;
    while(in.read_row(i_col, i_row, raw_str)) {
        i_cols.push_back(i_col);
        i_rows.push_back(i_row);
    }
    assert(i_cols.size()==outputs.size());
    std::vector<ygp::ColumnUID> col_uids;
    std::vector<ygp::RowIndex> row_idxs;
    auto n = dumped_outputs.size();
    int64_t i_output=0;
    for(decltype(n)i_dumped=0; i_dumped!=n; ++i_dumped){
        while(outputs[i_output]!=dumped_outputs[i_dumped]) ++i_output;
        if(i_dumped<10) fmt::print("{} {}\n", outputs[i_output],dumped_outputs[i_dumped]);
        assert(outputs[i_output]==dumped_outputs[i_dumped]);
        ChunkIndex chunk_idx{i_dumped};
        ygp::ColumnUID col_uid{i_cols[i_dumped]};
        ygp::RowIndex row_idx{i_rows[i_dumped]};
        col_uids.push_back(col_uid);
        row_idxs.push_back(row_idx);
    };

    auto filename = "ygp.h5";
    auto prefix = "ygp";

    generate_sent_uid(filename, prefix);
    write_voca_index_col(voca, filename, prefix);
    write_column(util::serialize(row_idxs), filename, prefix, ".chunk2row");
    write_column(util::serialize(col_uids), filename, prefix, ".chunk2col");
}

int main(int /*argc*/, char** argv){
    auto config = util::load_json(argv[1]);
//    pruning_voca();
//    convert_h5py_to_native();
//    write_WordUIDs("/home/jihuni/word2vec/ygp/words.uid", "test.Google.h5", "news.en.words", "news.en.uids");
//    write_WordUIDs("/home/jihuni/word2vec/ygp/words.uid", "s2010.h5", "s2010.words", "s2010.uids");
//    indexing_csv(argv[2]);
    //DepParsedTokens tokens{H5file{H5name{config["dep_parsed_store"].get<std::string>()},
    //                              hdf5::FileMode::read_exist}, config["dep_parsed_text"]};
//    QueryAndDumpCoreNLPoutput(argv[2]);
    //ParseWithCoreNLP(config, argv[2]);
//    GenerateExtraIndexes(config, argv[2], argv[3]);
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

