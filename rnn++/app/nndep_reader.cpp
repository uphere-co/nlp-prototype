#include <vector>
#include <algorithm>
#include <cctype>
#include <fstream>

#include <set>

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
                  std::string prefix, std::string colname,
                  hdf5::FileMode mode=hdf5::FileMode::rw_exist){
    H5file file{H5name{filename}, mode};
    file.writeRawData(H5name{prefix+colname}, rows);
}
void overwrite_column(std::vector<int64_t> rows, std::string filename,
                  std::string prefix, std::string colname){
    H5file file{H5name{filename}, hdf5::FileMode::rw_exist};
    file.overwriteRawData(H5name{prefix+colname}, rows);
}


void parse_json_dumps(nlohmann::json const &config,
                      const char *cols_to_exports, int64_t n_max=-1){
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
        auto n = n_max<0? body.size(): n_max;
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

struct CountryColumn{
    CountryColumn() {
        table2country_code["reach_reports"] = "country_code";
        table2country_code["regulation"] = "countrycode";
        table2country_code["autchklist2"] = "countrycode";
    }
    std::string operator[](std::string table) const {
        auto it = table2country_code.find(table);
        if(it==table2country_code.cend()) return "";
        return it->second;
    }
    std::map<std::string, std::string> table2country_code;
};

struct Chunks{
    using idx_t = std::pair<ChunkIndex,SentUID>;
    Chunks(util::io::H5file const &file, std::string prefix)
            : sents_uid{util::deserialize<SentUID>(file.getRawData<int64_t>(H5name{prefix+".sent_uid"}))},
              chunks_idx{util::deserialize<ChunkIndex>(file.getRawData<int64_t>(H5name{prefix+".chunk_idx"}))}
    {
        assert(sents_uid.size()==chunks_idx.size());
    }

    DPTokenIndex token_beg() const {return DPTokenIndex{0};}
    DPTokenIndex token_end() const {return DPTokenIndex::from_unsigned(sents_uid.size());}
    ChunkIndex chunk_idx(DPTokenIndex idx) const {return chunks_idx[idx.val];}
    SentUID sent_uid(DPTokenIndex idx) const {return sents_uid[idx.val];}
    idx_t at(DPTokenIndex idx) const {return {chunk_idx(idx),sent_uid(idx)};}

    DPTokenIndex next_sent_beg(DPTokenIndex idx) const {
        auto current_sent_beg = at(idx);
        auto end = token_end();
        auto it=idx;
        while(it<end) if(at(it++)!=current_sent_beg) break;
        return it;
    }
private:
    std::vector<ChunkIndex>   chunks_idx;
    std::vector<SentUID>      sents_uid;
};


void write_contry_code(nlohmann::json const &config,
                       const char *cols_to_exports, int64_t n_max = -1) {
    using namespace ygp;
    VocaInfo voca{config["wordvec_store"], config["voca_name"],
                  config["w2vmodel_name"], config["w2v_float_t"]};
    WordUIDindex wordUIDs{config["word_uids_dump"].get<std::string>()};

    auto output_filename = config["dep_parsed_store"].get<std::string>();
    auto prefix = config["dep_parsed_prefix"].get<std::string>();

    CountryColumn table2country_code{};

    H5file ygp_h5store{H5name{config["dep_parsed_store"].get<std::string>()},hdf5::FileMode::rw_exist};
    std::string ygp_prefix = config["dep_parsed_prefix"];
    RowUID row_uid{};
    YGPdb db{cols_to_exports};
    YGPindexer ygp_indexer{ygp_h5store, ygp_prefix};
    Chunks ygp_chunks{ygp_h5store, ygp_prefix};
    std::map<std::string, std::vector<RowUID>> rows_by_country;
    std::map<std::string, std::vector<SentUID>> sents_by_country;
    std::map<RowUID,std::vector<SentUID>> sents_in_row;

    for(auto idx=ygp_chunks.token_beg();idx!=ygp_chunks.token_end(); idx = ygp_chunks.next_sent_beg(idx)){
        auto ch_idx=ygp_chunks.chunk_idx(idx);
        auto sent_uid=ygp_chunks.sent_uid(idx);
        auto row_uid=ygp_indexer.row_uid(ch_idx);
        sents_in_row[row_uid].push_back(sent_uid);
        assert(row_uid.val==ch_idx.val);
    }

    for (auto col_uid = db.beg(); col_uid != db.end(); ++col_uid) {
        auto table = db.table(col_uid);
        auto column = db.column(col_uid);
        auto index_col = db.index_col(col_uid);
        auto country_code_col = table2country_code[table];
        fmt::print("{} {} {}\n", table, country_code_col, index_col);

        pqxx::connection C{"dbname=C291145_gbi_test host=bill.uphere.he"};
        pqxx::work W(C);
        auto query = fmt::format("SELECT {0}.{1},OT_country_code.country_name FROM {0}\
                                    INNER JOIN OT_country_code ON (OT_country_code.country_code = {0}.{2});",
                                 table, index_col, country_code_col);
        auto body = W.exec(query);
        W.commit();
        auto n = n_max < 0 ? body.size() : n_max;
        for (decltype(n) i = 0; i != n; ++i) {
            auto elm = body[i];
            RowIndex row_idx{std::stoi(elm[0].c_str())};
            std::string country = elm[1].c_str();
            if(ygp_indexer.is_empty(col_uid,row_idx)) continue;
            auto row_uid = ygp_indexer.row_uid(col_uid, row_idx);
            rows_by_country[country].push_back(row_uid);
            auto& tmp = sents_by_country[country];
            for(auto sent_uid : sents_in_row[row_uid]) tmp.push_back(sent_uid);
        }
    }

    std::ofstream country_list{config["country_uids_dump"].get<std::string>()};
    for(auto x : rows_by_country){
        ygp_h5store.writeRawData(H5name{x.first+".row_uid"}, util::serialize(x.second));
        country_list << x.first << std::endl;
    }
    for(auto x : sents_by_country){
        ygp_h5store.writeRawData(H5name{x.first+".sent_uid"}, util::serialize(x.second));
    }
    country_list.close();
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
        auto index_col = db.index_col(col_uid);
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




void test_contry_code(nlohmann::json const &config,
                      const char *cols_to_exports, int64_t n_max=-1){
//    using namespace ygp;
//    using idx_by_country_t = std::map<std::string, std::set<RowIndex>>;
//    std::map<ColumnUID, idx_by_country_t> country_indexer;
//    YGPdb db{cols_to_exports};
//    std::map<std::string, std::string> table2country_code;
//    table2country_code["reach_reports"] = "country_code";
//    table2country_code["regulation"] = "countrycode";
//    table2country_code["autchklist2"] = "countrycode";
//    for(auto col_uid =db.beg(); col_uid!=db.end(); ++col_uid){
//        auto table = db.table(col_uid);
//        auto column = db.column(col_uid);
//        auto index_col = db.index_col(col_uid);
//        auto country_code_col = table2country_code[table];
//        pqxx::connection C{"dbname=C291145_gbi_test host=bill.uphere.he"};
//        pqxx::work W(C);
//        auto query=fmt::format("SELECT {0}.{1},OT_country_code.country_name FROM {0}\
//                                    INNER JOIN OT_country_code ON (OT_country_code.country_code = {0}.{2});",
//                               tabale, index_col, country_code_col);
//        auto body= W.exec(query);
//        W.commit();
//        auto n = n_max<0? body.size(): n_max;
//        auto& mm = country_indexer[col_uid];
//        for(decltype(n)i=0; i!=n; ++i){
//            auto elm = body[i];
//            RowIndex row_idx{std::stoi(elm[0].c_str())};
//            std::string country = elm[1].c_str();
//            assert(mm[country].find(row_idx)!=mm[country].cend());
//            //fmt::print("{} {} : {} {}\n", table, row_idx.val, country, wordUIDs[country].val);
//        }
//    }
//    write_column(util::serialize(row_uids), output_filename, prefix, ".chunk2row");
}


namespace test {

void unicode_conversion(){
    auto row_str = u8"This is 테스트 of unicode-UTF8 conversion.";
    auto substr = util::string::substring_unicode_offset(row_str, 8, 11);
    assert(substr==u8"테스트");
    assert(substr!=u8"테스트 ");
    assert(substr!=u8" 테스트");
    fmt::print("{}\n", substr);
}

void word_importance(util::json_t const &config){
    WordUIDindex wordUIDs{config["word_uids_dump"].get<std::string>()};
    POSUIDindex const posUIDs{config["pos_uids_dump"].get<std::string>()};
    ArcLabelUIDindex const arclabelUIDs{config["arclabel_uids_dump"].get<std::string>()};
    WordImportance word_cutoff{H5file{H5name{config["word_prob_dump"].get<std::string>()},
                                      hdf5::FileMode::read_exist}};
    auto ask = util::load_json("query.unittest.inf_cutoff.corenlp");
    DepParsedTokens query_tokens{};
    query_tokens.append_corenlp_output(wordUIDs, posUIDs, arclabelUIDs, ask);
    query_tokens.build_sent_uid(SentUID{SentUID::val_t{0x80000000}});
    auto sents = query_tokens.IndexSentences();
    for(auto sent : sents){
        for(auto idx=sent.beg; idx!=sent.end; ++idx){
            auto wuid = sent.tokens->word_uid(idx);
            auto word = wordUIDs[wuid];
            auto cutoff = word_cutoff.cutoff(wuid);
            assert(cutoff == 0.0);
        }
    }
}



void test_chunks(){
}

}//namespace test

namespace test {
namespace ygp {

void country_annotator(util::json_t const &config) {
    using wordrep::ygp::CountryCodeAnnotator;
    CountryCodeAnnotator country_tagger{config["country_uids_dump"].get<std::string>()};
    {
        auto tags = country_tagger.tag("Seoul is a capital city of South Korea.\n");
        assert(!util::isin(tags, "Japan"));
        assert(util::isin(tags, "South Korea"));
    }
    {
        //Test a special logic that treats "Korea" as "South Korea"
        auto tags = country_tagger.tag("Seoul is a capital city of Korea.\n");
        assert(!util::isin(tags, "Japan"));
        assert(util::isin(tags, "South Korea"));
    }

    {
        //"China" should also includes "Hong Kong"
        auto tags = country_tagger.tag("China is in Asia.\n");
        assert(!util::isin(tags, "Japan"));
        assert(util::isin(tags, "China"));
        assert(util::isin(tags, "Hong Kong"));
    }
}


void YGPindexer(){
    //col_uid,row_idx -> row_uid;
}

}//namespace test::ygp
}//namespace test

int main(int /*argc*/, char** argv){
    auto config = util::load_json(argv[1]);
//    test::ygp::country_annotator(config);
//    test::word_importance(config);
//    test::unicode_conversion();
//    return 0;
//    auto query_result = util::load_json(argv[2]);
//    annotation_on_result(config, query_result);
//    fmt::print("{}\n", query_result.dump(4));
//    return 0;
//    auto col_uids = argv[2];
//    auto n_max = std::stoi(argv[3]);
//    write_contry_code(config, col_uids, n_max);
    //dump_psql(col_uids);
//    parse_json_dumps(config, col_uids, n_max);
//    return 0;
//    pruning_voca();
//    convert_h5py_to_native();
//    write_WordUIDs("/home/jihuni/word2vec/ygp/words.uid", "test.Google.h5", "news.en.words", "news.en.uids");
//    write_WordUIDs("/home/jihuni/word2vec/ygp/words.uid", "s2010.h5", "s2010.words", "s2010.uids");

    std::string input = argv[2];
    CoreNLPwebclient corenlp_client{config["corenlp_client_script"].get<std::string>()};
//    auto query_json = corenlp_client.from_query_content(input);
    auto query_str = util::string::read_whole(input);
    auto query_json = corenlp_client.from_query_content(query_str);
    query_json["query_str"] = query_str;

    util::Timer timer{};

    DepSimilaritySearch engine{config};
    timer.here_then_reset("Data loaded.");
    auto uids = engine.register_documents(query_json);
    uids["max_clip_len"] = query_json["max_clip_len"];
    //fmt::print("{}\n", uids.dump(4));
    auto answers = engine.ask_query(uids);
    ygp::annotation_on_result(config, answers);
    fmt::print("{}\n", answers.dump(4));
    fmt::print("\n\n--------- ------------\nA chain query find results:\n", answers.dump(4));
    auto chain_answers = engine.ask_chain_query(uids);
    ygp::annotation_on_result(config, chain_answers);
    fmt::print("{}\n", chain_answers.dump(4));
    timer.here_then_reset("Queries are answered.");
    return 0;
}
