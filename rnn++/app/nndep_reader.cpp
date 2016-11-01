#include <vector>
#include <algorithm>
#include <cctype>

#include "pqxx/pqxx"
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

void write_column(std::vector<int64_t> rows, std::string filename,
                  std::string prefix, std::string colname){
    H5file file{H5name{filename}, hdf5::FileMode::rw_exist};
    file.writeRawData(H5name{prefix+colname}, rows);
}
void overwrite_column(std::vector<int64_t> rows, std::string filename,
                  std::string prefix, std::string colname){
    H5file file{H5name{filename}, hdf5::FileMode::rw_exist};
    file.overwriteRawData(H5name{prefix+colname}, rows);
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

void QueryAndDumpCoreNLPoutput(const char* file, const char* dumpfile_prefix){
    CoreNLPwebclient corenlp_client{"../rnn++/scripts/corenlp.py"};
    io::CSVReader<3, io::trim_chars<' ', '\t'>, io::double_quote_escape<',', '"'>> in(file);
//    in.read_header(io::ignore_extra_column, "row_str");
    int64_t col_uid, row_idx;
    std::string row_str;
    std::vector<std::string> rows;
    while(in.read_row(col_uid, row_idx, row_str)) {
        rows.push_back(row_str);
        if (row_str.size() < 6) assert(0);
    }


    auto n = rows.size();
    tbb::parallel_for(tbb::blocked_range < decltype(n) > {0, n},
                      [&](tbb::blocked_range<decltype(n)> const &r) {
                          for (auto i = r.begin(); i != r.end(); ++i) {
                              auto const &row_str = rows[i];
                              auto const &parsed_json = corenlp_client.from_query_content(row_str);
                              std::ofstream temp_file;
                              temp_file.open(fmt::format(dumpfile_prefix, i));
                              temp_file << parsed_json.dump(4);
                              temp_file.close();
                          }
                      });
    return;
}
void ParseWithCoreNLP(nlohmann::json const &config, const char* raw_csv, const char* dumpfile_prefix) {
    VocaInfo voca{config["wordvec_store"], config["voca_name"],
                  config["w2vmodel_name"], config["w2v_float_t"]};
    WordUIDindex wordUIDs{config["word_uids_dump"].get<std::string>()};
    POSUIDindex posUIDs{config["pos_uids_dump"].get<std::string>()};
    ArcLabelUIDindex arclabelUIDs{config["arclabel_uids_dump"].get<std::string>()};

    auto output_filename = config["dep_parsed_store"].get<std::string>();
    auto prefix = config["dep_parsed_prefix"].get<std::string>();

    io::CSVReader<3, io::trim_chars<' ', '\t'>, io::double_quote_escape<',', '"'>> in(raw_csv);
    //in.read_header(io::ignore_extra_column, "row_str");
    int64_t i_col, i_row;
    std::string raw_str;
    std::vector<ygp::ColumnUID> col_uids;
    std::vector<ygp::RowIndex> row_idxs;
    std::vector<ygp::RowUID> row_uids;
    int64_t i_chunk=0;
    DepParsedTokens tokens{};
    //for(decltype(n_items)i=0; i!=n_items; ++i){
    while(in.read_row(i_col, i_row, raw_str)) {
        auto filename = fmt::format(dumpfile_prefix, i_chunk);
        ygp::ColumnUID col_uid{i_col};
        ygp::RowIndex row_idx{i_row};
        ygp::RowUID row_uid{i_chunk};
        ++i_chunk;
        std::ifstream f{filename};

        if(! std::ifstream{filename}.good()) {
            fmt::print("{} is missing.\n", filename);
            continue;
        }
        auto parsed_json = util::load_json(filename);
        if(parsed_json.size()==0) {
            fmt::print("{} has null contents.\n", filename);
            continue;
        }
        tokens.append_corenlp_output(wordUIDs, posUIDs, arclabelUIDs, parsed_json);
        col_uids.push_back(col_uid);
        row_idxs.push_back(row_idx);
        row_uids.push_back(row_uid);
    }
    tokens.build_sent_uid();
    tokens.write_to_disk(output_filename, prefix);

    write_column(util::serialize(row_uids), output_filename, prefix, ".chunk2row");
    write_column(util::serialize(row_idxs), output_filename, prefix, ".chunk2row_idx");
    write_column(util::serialize(col_uids), output_filename, prefix, ".chunk2col");
}

void GenerateExtraIndexes(nlohmann::json const &config) {
    VocaInfo voca{config["wordvec_store"], config["voca_name"],
                  config["w2vmodel_name"], config["w2v_float_t"]};

    auto filename = config["dep_parsed_store"].get<std::string>();
    auto prefix = config["dep_parsed_prefix"].get<std::string>();

    write_voca_index_col(voca, filename, prefix);
}

int psql(){
    try
    {
        pqxx::connection C{"dbname=C291145_gbi_test"};
        std::cout << "Connected to " << C.dbname() << std::endl;
        pqxx::work W(C);

        pqxx::result R = W.exec("SELECT table_name FROM information_schema.tables WHERE table_schema = 'public';");

        std::cout << "Found " << R.size() << "Tables:" << std::endl;
        for (auto row: R)
            std::cout << row[0].c_str() << std::endl;

        W.commit();
        std::cout << "ok." << std::endl;
    }
    catch (const std::exception &e)
    {
        std::cerr << e.what() << std::endl;
        return 1;
    }
    return 0;
}

int main(int /*argc*/, char** argv){
    psql();
    return 0;
    auto config = util::load_json(argv[1]);
//    pruning_voca();
//    convert_h5py_to_native();
//    write_WordUIDs("/home/jihuni/word2vec/ygp/words.uid", "test.Google.h5", "news.en.words", "news.en.uids");
//    write_WordUIDs("/home/jihuni/word2vec/ygp/words.uid", "s2010.h5", "s2010.words", "s2010.uids");
//    indexing_csv(argv[2]);
    //DepParsedTokens tokens{H5file{H5name{config["dep_parsed_store"].get<std::string>()},
    //                              hdf5::FileMode::read_exist}, config["dep_parsed_text"]};

//    auto csvfile = argv[2];
//    const char* dumpfile_prefix = argv[3]; //"/home/jihuni/nlp-prototype/build/corenlp/row.{:06}"
//    QueryAndDumpCoreNLPoutput(csvfile, dumpfile_prefix);
//    ParseWithCoreNLP(config, csvfile, dumpfile_prefix);
//    GenerateExtraIndexes(config);
//    return 0;
    std::string input = argv[2];
    CoreNLPwebclient corenlp_client{config["corenlp_client_script"].get<std::string>()};
//    auto query_json = corenlp_client.from_query_content(input);
    auto query_json = corenlp_client.from_query_file(input);

    util::Timer timer{};
    DepSimilaritySearch engine{config};
    timer.here_then_reset("Data loaded.");
//    auto answer = engine.process_queries(query_json);
    auto answer = engine.process_queries(query_json);
    timer.here_then_reset("Queries are answered.");
    fmt::print("{}\n", answer.dump(4));
    return 0;
}

