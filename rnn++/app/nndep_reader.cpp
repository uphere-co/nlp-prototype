#include <vector>
#include <algorithm>
#include <cctype>
#include <fstream>
#include <set>

#include "pqxx/pqxx"
#include "fmt/printf.h"

#include "similarity/dep_similarity.h"
#include "data_source/ygp_db.h"
#include "data_source/ygp_etl.h"
#include "data_source/corenlp_helper.h"

#include "utils/hdf5.h"
#include "utils/profiling.h"
#include "utils/string.h"

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


}//namespace test

namespace ygp {
namespace test {

void test_chunks(){
}

void country_annotator(util::json_t const &config) {
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


void ygp_indexing(){
    //col_uid,row_idx -> row_uid;
}

void country_code(util::json_t  const &config){
    WordUIDindex wordUIDs{config["word_uids_dump"].get<std::string>()};
    using idx_by_country_t = std::map<std::string, std::set<RowIndex>>;
    std::map<ColumnUID, idx_by_country_t> country_indexer;
    YGPdb db{config["column_uids_dump"].get<std::string>()};
    H5file ygp_h5store{H5name{config["dep_parsed_store"].get<std::string>()},
                       hdf5::FileMode::rw_exist};
    std::string ygp_prefix = config["dep_parsed_prefix"];
    YGPindexer ygp_indexer{ygp_h5store, ygp_prefix};
    ygp::DBbyCountry ygpdb_country{ygp_h5store, config["country_uids_dump"].get<std::string>()};
    DepParsedTokens tokens{ygp_h5store, ygp_prefix};

    auto sents=tokens.IndexSentences();
    int i=0;
    for(auto sent : sents) {
        if(i++>1000) break;
        auto chunk_idx = tokens.chunk_idx(sent.beg);
        auto col_uid = ygp_indexer.column_uid(chunk_idx);
        auto row_idx = ygp_indexer.row_idx(chunk_idx);
        auto row_uid = ygp_indexer.row_uid(chunk_idx);
        data::ygp::CountryColumn table2country_code{};
        auto table = db.table(col_uid);
        auto column = db.column(col_uid);
        auto index_col = db.index_col(col_uid);
        auto country_code_col = table2country_code[table];
        pqxx::connection C{"dbname=C291145_gbi_test host=bill.uphere.he"};
        pqxx::work W(C);
        auto query = fmt::format("SELECT {1},OT_country_code.country_name FROM {0}\
                                  INNER JOIN OT_country_code ON (OT_country_code.country_code = {0}.{2})\
                                  WHERE {0}.{1}={3};",
                                 table, index_col, country_code_col, row_idx.val);
        auto body = W.exec(query);
        W.commit();
        auto n = body.size();
        for(decltype(n)j=0; j!=n; ++j){
            auto elm = body[j];
            assert(row_idx==RowIndex{std::stoi(elm[0].c_str())});
            std::string country = elm[1].c_str();
            if(country != ygpdb_country.get_country(sent.uid))
                std::cerr<<fmt::format("{} : {} {} {} {}.uid {}.sent_uid {} {}", i,table, column, row_idx.val,
                                       row_uid.val, sent.uid.val,
                                       country, ygpdb_country.get_country(sent.uid))<<std::endl;
            assert(country == ygpdb_country.get_country(sent.uid));
        }
    }
}


}//namespace ygp::test
}//namespace ygp

int main(int /*argc*/, char** argv){
    auto config = util::load_json(argv[1]);
//    ygp::test::country_annotator(config);
//    ygp::test::country_code(config);
//    test::word_importance(config);
//    test::unicode_conversion();
//    return 0;
//    auto col_uids = argv[2];
//    data::ygp::dump_psql(col_uids);
//    data::ygp::parse_json_dumps(config, config["column_uids_dump"].get<std::string>(), 100);
//    data::ygp::write_country_code(config, config["column_uids_dump"].get<std::string>());
//    return 0;
//    auto query_result = util::load_json(argv[2]);
//    annotation_on_result(config, query_result);
//    fmt::print("{}\n", query_result.dump(4));
//    return 0;
//    write_contry_code(config, col_uids, n_max);
//    return 0;
//    pruning_voca();
//    convert_h5py_to_native();
//    write_WordUIDs("/home/jihuni/word2vec/ygp/words.uid", "test.Google.h5", "news.en.words", "news.en.uids");
//    write_WordUIDs("/home/jihuni/word2vec/ygp/words.uid", "s2010.h5", "s2010.words", "s2010.uids");

    std::string input = argv[2];
    data::CoreNLPwebclient corenlp_client{config["corenlp_client_script"].get<std::string>()};
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
    timer.here_then_reset("Registered documents.");
    auto answers = engine.ask_query(uids);
    timer.here_then_reset("Processed a query.");
    ygp::annotation_on_result(config, answers);
    timer.here_then_reset("Query output annotation.");
    fmt::print("{}\n", answers.dump(4));
    fmt::print("\n\n--------- ------------\nA chain query find results:\n", answers.dump(4));
    timer.here_then_reset("Begin a chain query.");
    auto chain_answers = engine.ask_chain_query(uids);
    timer.here_then_reset("Processed a chain query.");
    ygp::annotation_on_result(config, chain_answers);
    timer.here_then_reset("A chain query output annotatoin.");
    fmt::print("{}\n", chain_answers.dump(4));
    timer.here_then_reset("Queries are answered.");
    return 0;
}
