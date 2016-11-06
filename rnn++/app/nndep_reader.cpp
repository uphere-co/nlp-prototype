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


void parse_json_dumps(nlohmann::json const &config, const char *cols_to_exports){
    VocaInfo voca{config["wordvec_store"], config["voca_name"],
                  config["w2vmodel_name"], config["w2v_float_t"]};
    WordUIDindex wordUIDs{config["word_uids_dump"].get<std::string>()};
    POSUIDindex posUIDs{config["pos_uids_dump"].get<std::string>()};
    ArcLabelUIDindex arclabelUIDs{config["arclabel_uids_dump"].get<std::string>()};

    auto output_filename = config["dep_parsed_store"].get<std::string>();
    auto prefix = config["dep_parsed_prefix"].get<std::string>();

    std::vector<ygp::ColumnUID> col_uids;
    std::vector<ygp::RowIndex> row_idxs;
    std::vector<ygp::RowUID> row_uids;
    DepParsedTokens tokens{};
    ygp::ColumnUID col_uid{};
    ygp::RowUID row_uid{};

    ygp::YGPdb db{cols_to_exports};
    for(auto col_uid =db.beg(); col_uid!=db.end(); ++col_uid){
        auto table = db.table(col_uid);
        auto column = db.column(col_uid);
        auto index_col = db.index_col(col_uid);

        pqxx::connection C{"dbname=C291145_gbi_test host=bill.uphere.he"};
        pqxx::work W(C);
        auto query=fmt::format("SELECT {} FROM {};", index_col, table);
        auto body= W.exec(query);
        W.commit();
        auto n = body.size();
        for(decltype(n)i=0; i!=n; ++i){
            auto row = body[i];
            auto index = std::stoi(row[0].c_str());
            auto dumpfile_name=fmt::format("corenlp/{}.{}.{}.{}", table, column, index_col, index);
            if(! std::ifstream{dumpfile_name}.good()) {
                fmt::print("{} is missing.\n", dumpfile_name);
                continue;
            }
            auto parsed_json = util::load_json(dumpfile_name);
            if(parsed_json.size()==0) {
                fmt::print("{} has null contents.\n", dumpfile_name);
                continue;
            }
            fmt::print("{} is found.\n", dumpfile_name);

            ygp::RowIndex row_idx{ygp::RowIndex::val_t{index}};
            tokens.append_corenlp_output(wordUIDs, posUIDs, arclabelUIDs, parsed_json);
            col_uids.push_back(col_uid);
            row_idxs.push_back(row_idx);
            row_uids.push_back(row_uid);

            ++row_uid;
        }
    }
    tokens.build_sent_uid(SentUID{SentUID::val_t{0}});
    tokens.build_voca_index(voca.indexmap);
    tokens.write_to_disk(output_filename, prefix);

    write_column(util::serialize(row_uids), output_filename, prefix, ".chunk2row");
    write_column(util::serialize(row_idxs), output_filename, prefix, ".chunk2row_idx");
    write_column(util::serialize(col_uids), output_filename, prefix, ".chunk2col");
    wordUIDs.write_to_disk(config["word_uids_dump"].get<std::string>());
}

int dump_column(std::string table, std::string column, std::string index_col){
    CoreNLPwebclient corenlp_client{"../rnn++/scripts/corenlp.py"};
    try
    {
        pqxx::connection C{"dbname=C291145_gbi_test host=bill.uphere.he"};
        std::cout << "Connected to " << C.dbname() << std::endl;
        pqxx::work W(C);


        auto query=fmt::format("SELECT {}, {} FROM {};", column, index_col, table);
        auto body= W.exec(query);
        W.commit();
        auto n = body.size();
        tbb::parallel_for(decltype(n){0}, n, [&](auto i) {
            auto row = body[i];
            std::string raw_text = row[0].c_str();
            auto index = row[1];
            auto dumpfile_name=fmt::format("corenlp/{}.{}.{}.{}", table, column, index_col, index);
            if(raw_text.size()<6) {
                fmt::print("{} is missing.\n", dumpfile_name);
                return;
            }
            //fmt::print("{:<6} : {} --------------------------\n{}",  index, dumpfile_name, raw_text);
            auto const &parsed_json = corenlp_client.from_query_content(raw_text);
            std::ofstream dump_file;
            dump_file.open(dumpfile_name);
            dump_file << parsed_json.dump(4);
            dump_file.close();
        });
    }
    catch (const std::exception &e)
    {
        std::cerr << e.what() << std::endl;
        return 1;
    }
    return 0;
}

void dump_psql(const char *cols_to_exports){
    ygp::YGPdb db{cols_to_exports};
    for(auto col_uid =db.beg(); col_uid!=db.end(); ++col_uid){
        auto table = db.table(col_uid);
        auto column = db.column(col_uid);
        auto index_col = db.column(col_uid);
        std::cerr<<fmt::format("Dumping : {:15} {:15} {:15}\n", table, column, index_col)<<std::endl;
        dump_column(table, column, index_col);
    }
}

int list_columns(){
    try
    {
        pqxx::connection C{"dbname=C291145_gbi_test host=bill.uphere.he"};
        std::cout << "Connected to " << C.dbname() << std::endl;
        pqxx::work W(C);

        pqxx::result R = W.exec("SELECT table_name FROM information_schema.tables WHERE table_schema = 'public';");

        std::cout << "Found " << R.size() << "Tables:" << std::endl;
        for (auto row: R) {
            for(auto elm : row) fmt::print("{} ",  elm);
            fmt::print("\n");
            auto table = row[0];
            auto query=fmt::format("select column_name from information_schema.columns where table_name='{}';", table);
            pqxx::result R = W.exec(query);
            for (auto column: R) fmt::print("{} ",  column[0]);
        }

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
    auto config = util::load_json(argv[1]);
//    auto query_result = util::load_json(argv[2]);
//    annotation_on_result(config, query_result);
//    fmt::print("{}\n", query_result.dump(4));
//    return 0;
    auto col_uids = argv[2];
//    dump_psql(col_uids);
    parse_json_dumps(config, col_uids);
    return 0;
//    pruning_voca();
//    convert_h5py_to_native();
//    write_WordUIDs("/home/jihuni/word2vec/ygp/words.uid", "test.Google.h5", "news.en.words", "news.en.uids");
//    write_WordUIDs("/home/jihuni/word2vec/ygp/words.uid", "s2010.h5", "s2010.words", "s2010.uids");

    std::string input = argv[2];
    CoreNLPwebclient corenlp_client{config["corenlp_client_script"].get<std::string>()};
//    auto query_json = corenlp_client.from_query_content(input);
    auto query_json = corenlp_client.from_query_file(input);

    util::Timer timer{};
    DepSimilaritySearch engine{config};
    timer.here_then_reset("Data loaded.");
    auto uids = engine.register_documents(query_json);
    uids["max_clip_len"] = query_json["max_clip_len"];
    fmt::print("{}\n", uids.dump(4));
    auto answers = engine.process_query(uids);
    ygp::annotation_on_result(config, answers);
    fmt::print("{}\n", answers.dump(4));
    timer.here_then_reset("Queries are answered.");
    return 0;
}

