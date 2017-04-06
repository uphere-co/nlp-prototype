#include <fmt/printf.h>

#include "data_source/rss.h"
#include "data_source/corenlp_helper.h"
#include "data_source/corenlp_utils.h"

#include "utils/hdf5.h"
#include "utils/profiling.h"
#include "utils/string.h"
#include "utils/persistent_vector.h"

#include "utils/filesystem.h"

#include "similarity/rss.h"

namespace data{
namespace rss{
namespace test {

void rss_corenlp_path(){
    std::cerr<<"Run rss::test::rss_corenlp_path " <<std::endl;
    {
        auto fullpath = "/home/jihuni/word2vec/nyt.corenlp/NYT.1.maintext.corenlp";
        RSSRowFilePath path{fullpath};
        assert(path.table =="NYT");
        assert(path.column=="maintext");
        assert(path.index  == 1);
    }
    {
        auto fullpath = "/foo/bar/WP.2.title.corenlp";
        RSSRowFilePath path{fullpath};
        assert(path.table =="WP");
        assert(path.column=="title");
        assert(path.index  ==2);
    }

    fmt::print(std::cerr, "Run data::rss::test::parse_input\n");
    RSSRowFilePath test{"/foo/bar/tests/NYT.10.maintext.corenlp"};
    assert(test.table=="NYT");
    assert(test.column=="maintext");
    assert(test.index==10);

    RSSRowFilePath test2{"tests/NYT.10.maintext.corenlp"};
    RSSRowFilePath test3{"NYT.10.maintext.corenlp"};
    RSSRowFilePath test_n{"NYT.11.maintext.corenlp"};
    assert(test==test2);
    assert(test==test3);
    assert(test!=test_n);
}

void IndexUIDs(util::json_t const& config){
    Factory factory{{config}};
    std::cerr<<"Run rss::test::IndexUIDs " <<std::endl;
    Columns rssdb = factory.db();
    auto tmp = rssdb.col_uid("title");
    fmt::print(std::cerr, "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! {}\n", tmp.val);
    for(int i=0; i<3; ++i)
        fmt::print(std::cerr, "!!!!!!!!{} {}\n", rssdb.col_uid(rssdb.column(i)).val, rssdb.column(i));
    assert(rssdb.col_uid("title") == data::ColumnUID{0});
    assert(rssdb.col_uid("summary") == data::ColumnUID{1});
    assert(rssdb.col_uid("maintext") == data::ColumnUID{2});
}

void rss_indexing(util::json_t const &config) {
    Factory factory{{config}};
    wordrep::DepParsedTokens tokens = factory.common.dep_parsed_tokens();
    wordrep::WordUIDindex wordUIDs  = factory.common.word_uid_index();
    auto sents = tokens.IndexSentences();
    auto sent = sents[563];
    auto chunk_idx = tokens.chunk_idx(sent.front());

    data::DBIndexer const db_indexer = factory.common.db_indexer();
    //auto row_uid = db_indexer.row_uid(chunk_idx);//if a chunk is a row, chunk_idx is row_uid
    auto col_uid = db_indexer.column_uid(chunk_idx);
    auto row_idx = db_indexer.row_idx(chunk_idx);
    std::map<data::ColumnUID, std::string> uid2col;
    uid2col[0] = "title";
    uid2col[1] = "summary";
    uid2col[2] = "maintext";

    auto rawtext_dir     = util::get_str(config,"rawtext_dir");
    auto rssdb = factory.db();
    auto table_name  = rssdb.table(col_uid);
    auto column_name = rssdb.column(col_uid);
    auto file_name = get_row_filename(table_name, column_name, row_idx.val);

    auto full_path = fmt::format(fmt::format("{}/{}", rawtext_dir, file_name));
    std::cerr << full_path << std::endl;
    auto row_str = util::string::read_whole(full_path);
    auto offset_beg = sent.beg_offset();
    auto offset_end = sent.end_offset();
    std::cerr << fmt::format("{}:{}", offset_beg.val, offset_end.val) << std::endl;
    auto    substr = util::string::substring_unicode_offset(row_str, offset_beg.val, offset_end.val);
    std::cerr << substr << std::endl;
    std::cerr << sent.repr(wordUIDs) << std::endl;
    std::cerr << std::endl;
}

void extract_filename(){
    fmt::print(std::cerr, "Run data::rss::test::extract_filename\n");
    assert(util::file::get_filename("/foo/bar/tests/filename")=="filename");
    assert(util::file::get_filename("tests/filename")=="filename");
    assert(util::file::get_filename("filename")=="filename");
    assert(util::file::get_filename("filename")!="name");
}

void test_all(int argc, char** argv){
    assert(argc > 1);
    auto config = util::load_json(argv[1]);
    rss_corenlp_path();
    IndexUIDs(config);
    rss_indexing(config);
    extract_filename();
}


}//namespace data::rss::test;
}//namespace data::rss;
}//namespace data;

int process_rss_dump(int argc, char** argv){
    assert(argc>1);
    auto config = util::load_json(argv[1]);
    util::Timer timer;

    auto json_dump_path   = argv[2];
    auto dataset_prefix   = argv[3];

    data::CoreNLPoutputParser dump_parser;
    auto json_dumps = util::string::readlines(json_dump_path);
    timer.here_then_reset(fmt::format("Begin to process {} JSON dump files. ",json_dumps.size()));
    data::parallel_load_jsons(json_dumps, dump_parser);
    timer.here_then_reset(fmt::format("Parsed {} files. ",dump_parser.chunks.size()));
    auto tokens = dump_parser.get();
    auto non_null_idxs = dump_parser.get_nonnull_idx();
    timer.here_then_reset("Parsing is finished. ");

//    wordrep::VocaInfo voca;
//    tokens.build_voca_index(voca.indexmap);
//    timer.here_then_reset("Built VocaIndex");


    tokens.to_file({dataset_prefix});
    data::rss::write_column_indexes(util::get_str(config, "column_uids_dump"),
                                    json_dump_path,
                                    non_null_idxs,
                                    dataset_prefix);
    return 0;
}

int main(int argc, char** argv){
//    data::rss::test::test_all(argc,argv);
//    return 0;

    process_rss_dump(argc, argv);
    return 0;
}
