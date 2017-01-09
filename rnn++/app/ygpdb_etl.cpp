#include <vector>
#include <algorithm>
#include <cctype>
#include <fstream>
#include <set>

#include <pqxx/pqxx>
#include <fmt/printf.h>

#include "similarity/query_engine.h"
#include "data_source/rss.h"
#include "data_source/ygp_db.h"
#include "data_source/ygp_etl.h"
#include "data_source/corenlp_helper.h"
#include "data_source/corenlp_utils.h"

#include "utils/hdf5.h"
#include "utils/profiling.h"
#include "utils/string.h"
#include "utils/type_param.h"
#include "utils/persistent_vector.h"
#include "utils/versioned_name.h"
#include "utils/optional.h"
#include "utils/math.h"

using namespace util::io;
using namespace wordrep;
using namespace engine;

using util::get_str;

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
    auto h5store = "/home/jihuni/word2vec/rss/news.h5";
    auto voca_words_name = "news.en.uids";
    auto voca_vecs_name = "news.en.vecs";
    auto voca_float_typename = "float32";
    auto file_words = "test.voca";
    WordUIDindex all_wordUIDs("/home/jihuni/word2vec/rss/all_words");

    auto all_word_uids = load_voca(h5store, voca_words_name);
    VocaIndexMap all_voca{all_word_uids};

    std::set<WordUID> all_words{all_word_uids.cbegin(),all_word_uids.cend()};

    WordUIDindex wordUIDs(file_words);
    auto uids = wordUIDs.get_uids();
    uids.push_back(the_unknown_word_uid());

    auto known_uids = util::filter(uids, [&all_words](auto uid){return all_words.find(uid)!=all_words.end();});
    VocaIndexMap voca{known_uids};

    auto n_words = known_uids.size();

    using WordBlock = WordBlock_base<float,100>;
    WordBlock all_wvecs{load_raw_wvec(h5store, voca_vecs_name, voca_float_typename)};

    //WordBlock wvecs{std::vector<WordBlock::val_t>(WordBlock::dim * n_words)};
    std::vector<WordBlock::val_t> raw_vecs;
    raw_vecs.reserve(WordBlock::dim * n_words);
    for(VocaIndex i=0; i!=VocaIndex::from_unsigned(n_words); ++i){
        auto word_uid = voca[i];
        auto a=all_wvecs[all_voca[word_uid]];
        std::copy(a.cbegin(),a.cend(), std::back_inserter(raw_vecs));
        //auto b = wvecs[i];
        //b = a;
//        for(int j=0; j!=100; ++j) b[j]==a[j];
        //for(int j=0; j!=100; ++j) assert(a[j]==b[j]);
    }
    WordBlock wvecs{std::move(raw_vecs)};
    H5file outfile{H5name{"test.h5"}, hdf5::FileMode::replace};
    util::TypedPersistentVector<WordUID> new_uids{voca_words_name, std::move(known_uids)};
    new_uids.write(outfile);
    outfile.writeRawData({voca_vecs_name},  util::serialize(wvecs.serialize()));
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

void parse_textfile(std::string dump_files){
    auto files = util::string::readlines(dump_files);
    auto n = files.size();
    tbb::parallel_for(decltype(n){0},n, [&](auto const &i) {
        auto file = files[i];
        data::CoreNLPwebclient corenlp_webclient("../rnn++/scripts/corenlp.py");
        corenlp_webclient.from_query_file(file);
    });
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
    WordImportance word_importance{H5file{H5name{config["word_prob_dump"].get<std::string>()},
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
            auto cutoff = word_importance.score(wuid);
            assert(cutoff == 0.0);
        }
    }
}

using util::PersistentVector;
using util::TypedPersistentVector;

void persistent_vector_float(){
    std::vector<float> vals = {1.1, 1.2, 1.3, 1.4, 1.5, 1.6};
    PersistentVector<float,float> vec{vals, "reals"};
    {
        H5file h5store{H5name{"tmp.1849ya98fy2qhrqr6y198r1yr.h5"}, hdf5::FileMode::replace};
        vec.write(h5store);
    }
    {
        H5file h5store{H5name{"tmp.1849ya98fy2qhrqr6y198r1yr.h5"}, hdf5::FileMode::read_exist};
        PersistentVector<float,float> vec2{h5store, "reals"};
        for(auto x : util::zip(vec, vec2)) assert(x.first ==x.second);
    }
}
void persistent_vector_WordUID(){
    std::vector<WordUID::val_t> vals = {932,4,1,3,10};
    TypedPersistentVector<WordUID> vec{vals, "wuid"};
    {
        H5file h5store{H5name{"tmp.1849ya98fy2qhrqr6y198r1yr.h5"}, hdf5::FileMode::replace};
        vec.write(h5store);
    }
    {
        H5file h5store{H5name{"tmp.1849ya98fy2qhrqr6y198r1yr.h5"}, hdf5::FileMode::read_exist};
        TypedPersistentVector<WordUID> vec2{h5store, "wuid"};
        for(auto x : util::zip(vec, vec2)) assert(x.first ==x.second);
    }

    //test copy
    auto vec3 = vec;
    for(auto x : util::zip(vec, vec3)) assert(x.first ==x.second);
    vec3.front() = WordUID{1};
    assert(vec3.front()!=vec.front());
}



void filesystem(util::json_t const &config){
    auto data_path = util::get_str(config, "dep_parsed_store");
    {
        std::vector <std::string> files = {"foo.h5.1.0", "foo.h5.1.1", "foo.h5.1.2", "foo.h5.1.10", "foo.h5.1.3"};
        auto ver = util::get_latest_version(files);
        assert(ver.minor == 10);
        assert(ver.major == 1);
        assert(ver.name == "foo.h5");
    }

    {
        std::vector<std::string> files = {"/bar/foo.h5.1.0", "/bar/foo.h5.2.1"};
        auto ver = util::get_latest_version(files);
        assert(ver.minor == 1);
        assert(ver.major == 2);
        assert(ver.name == "/bar/foo.h5");
        assert(ver.fullname == "/bar/foo.h5.2.1");
    }

    std::cerr<< util::get_latest_version(data_path).fullname << std::endl;
}

}//namespace test

namespace data {
namespace ygp {
namespace test {

void ygpdb_indexing(util::json_t const &config){
    using util::string::split;
    auto path = "corenlp/autchklist2.guidenote.autchkid.12668";
    RowDumpFilePath row{path};
    assert(row.table    =="autchklist2");
    assert(row.column   =="guidenote");
    assert(row.index_col=="autchkid");
    assert(row.index    == 12668);
    assert(row.full_column_name() == "autchklist2.guidenote.autchkid");

    auto cols_to_exports = config["column_uids_dump"].get<std::string>();
    YGPdb db{cols_to_exports};
    assert(db.col_uid("regulation.regtitle.regid")==ColumnUID{3});
    assert(db.col_uid("reach_reports.content.report_id")==ColumnUID{0});
    assert(db.is_in("reach_reports.content.report_id"));
    assert(!db.is_in("dsafasfafsafa.content.asfasdf"));
}

void chunks() {
}

void country_annotator(util::json_t const &config) {
    CountryCodeAnnotator country_tagger{
            util::get_latest_version(util::get_str(config, "country_uids_dump")).fullname};
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


void country_code(util::json_t const &config) {
    WordUIDindex wordUIDs{config["word_uids_dump"].get<std::string>()};
    using idx_by_country_t = std::map<std::string, std::set<RowIndex>>;
    std::map<ColumnUID, idx_by_country_t> country_indexer;
    YGPdb db{config["column_uids_dump"].get<std::string>()};
    H5file ygp_h5store{H5name{config["dep_parsed_store"].get<std::string>()},
                       hdf5::FileMode::rw_exist};
    std::string ygp_prefix = config["dep_parsed_prefix"];
    DBIndexer ygp_indexer{ygp_h5store, ygp_prefix};
    DBbyCountry ygpdb_country{ygp_h5store,
                              util::get_latest_version(util::get_str(config, "country_uids_dump")).fullname};
    DepParsedTokens tokens{ygp_h5store, ygp_prefix};

    auto sents = tokens.IndexSentences();
    int i = 0;
    for (auto sent : sents) {
        if (i++ > 1000) break;
        auto chunk_idx = tokens.chunk_idx(sent.beg);
        auto col_uid = ygp_indexer.column_uid(chunk_idx);
        auto row_idx = ygp_indexer.row_idx(chunk_idx);
        auto row_uid = ygp_indexer.row_uid(chunk_idx);
        CountryColumn table2country_code{};
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
        for (decltype(n) j = 0; j != n; ++j) {
            auto elm = body[j];
            assert(row_idx == RowIndex{std::stoi(elm[0].c_str())});
            std::string country = elm[1].c_str();
            if (country != ygpdb_country.get_country(sent.uid))
                std::cerr << fmt::format("{} : {} {} {} {}.uid {}.sent_uid {} {}", i, table, column, row_idx.val,
                                         row_uid.val, sent.uid.val,
                                         country, ygpdb_country.get_country(sent.uid)) << std::endl;
            assert(country == ygpdb_country.get_country(sent.uid));
        }
    }
}


}//namespace data::ygp::test
}//namespace data::ygp
}//namespace data

namespace data{
namespace rss{
namespace test {

void rss_corenlp_path(){
    std::cerr<<"Run rss::test::rss_corenlp_path " <<std::endl;
    {
        auto fullpath = "/home/jihuni/word2vec/nyt.corenlp/672b36bc0531fb33c3d324ed7528d8b42c9082b4ff7b8ef07a33d043752aeae3.maintext.corenlp";
        RSSRowFilePath path{fullpath};
        assert(path.table =="nyt");
        assert(path.column=="maintext");
        assert(path.hash  =="672b36bc0531fb33c3d324ed7528d8b42c9082b4ff7b8ef07a33d043752aeae3");
    }
    {
        auto fullpath = "/home/jihuni/word2vec/NYT.corenlp/672b36bc0531fb33c3d324ed7528d8b42c9082b4ff7b8ef07a33d043752aeae3.maintext.corenlp";
        RSSRowFilePath path{fullpath};
        assert(path.table =="NYT");
        assert(path.column=="maintext");
        assert(path.hash  =="672b36bc0531fb33c3d324ed7528d8b42c9082b4ff7b8ef07a33d043752aeae3");
    }
}

void IndexUIDs(util::json_t const& config){
    std::cerr<<"Run rss::test::IndexUIDs " <<std::endl;
    Columns rssdb{config["column_uids_dump"].get<std::string>()};
    auto tmp = rssdb.col_uid("title");
    fmt::print(std::cerr, "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! {}\n", tmp.val);
    for(int i=0; i<3; ++i)
        fmt::print(std::cerr, "!!!!!!!!{} {}\n", rssdb.col_uid(rssdb.column(i)).val, rssdb.column(i));
    assert(rssdb.col_uid("title") == data::ColumnUID{0});
    assert(rssdb.col_uid("summary") == data::ColumnUID{1});
    assert(rssdb.col_uid("maintext") == data::ColumnUID{2});
}

void rss_indexing(util::json_t const &config) {
    std::string hashes = util::get_str(config,"row_hashes");
    wordrep::DepParsedTokens tokens{
            util::get_latest_version(util::get_str(config, "dep_parsed_store")),
            config["dep_parsed_prefix"]};
    wordrep::WordUIDindex wordUIDs{config["word_uids_dump"].get<std::string>()};
    auto sents = tokens.IndexSentences();
    auto sent = sents[563];
    auto chunk_idx = tokens.chunk_idx(sent.beg);

    data::DBIndexer const db_indexer{
            h5read(util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname),
            config["dep_parsed_prefix"].get<std::string>()};
    //auto row_uid = db_indexer.row_uid(chunk_idx);//if a chunk is a row, chunk_idx is row_uid
    auto col_uid = db_indexer.column_uid(chunk_idx);
    auto row_idx = db_indexer.row_idx(chunk_idx);
    std::map<data::ColumnUID, std::string> uid2col;
    uid2col[0] = "title";
    uid2col[1] = "summary";
    uid2col[2] = "maintext";
    data::rss::HashIndexer hash2idx{hashes};
    //TODO: remove hard-coded path.
    auto filename = fmt::format("/home/jihuni/word2vec/NYT.text/{}.{}", hash2idx.hash(row_idx.val), uid2col[col_uid]);
    auto row_str = util::string::read_whole(filename);
    std::cerr << filename << std::endl;
    auto offset_beg = sent.beg_offset();
    auto offset_end = sent.end_offset();
    std::cerr << fmt::format("{}:{}", offset_beg.val, offset_end.val) << std::endl;
    auto    substr = util::string::substring_unicode_offset(row_str, offset_beg.val, offset_end.val);
    std::cerr << substr << std::endl;
    std::cerr << sent.repr(wordUIDs) << std::endl;
    std::cerr << std::endl;
}


}//namespace data::rss::test
}//namespace data::rss
}//namespace data

namespace data {
namespace corenlp {
namespace test {

struct Word{
    Word(){};
    Word(std::string substr) {
        auto dep_word_end = substr.find_last_of("-");
        word = substr.substr(0, dep_word_end);
        idx = std::stoi(substr.substr(dep_word_end+1))-1;
    }
    std::string word;
    int64_t idx;
};

struct Offset{
    int64_t beg;
    int64_t end;
};
struct WordToken{
    WordToken(std::string line, int64_t idx){
        auto elms = util::string::split(line.substr(1, line.size()-2), " ");
        auto get = [](auto elm){return elm.substr(elm.find("=")+1);};
        self.word = get(elms[0]);
        self.idx  = idx;
        offset.beg =  std::stoi(get(elms[1]));
        offset.end =  std::stoi(get(elms[2]));
        pos = get(elms[3]);
    }
    std::string pos;
    Word self;
    Offset offset;
};
struct DepToken{
    DepToken(std::string line){
        auto label_end = line.find("(");
        auto dep_end = line.find(", ");
        auto gov_end = line.find(")");
        arclabel  = line.substr(0, label_end);
        dependent = Word{line.substr(1+label_end, dep_end - (1+label_end))};
        governor  = Word{line.substr(2+dep_end, gov_end- (2+dep_end))};
    }
    std::string arclabel;
    Word dependent;
    Word governor;
};

bool isSentBeg(std::string const &sentence){
    std::string tag = "Sentence #";
    return tag == sentence.substr(0,tag.size());
}
bool isSentEnd(std::string const &sentence){
    std::string tag = "";
    return tag == sentence;
}
struct DepChunk{
    using lines_t = std::vector<std::string>::const_iterator;

    static std::optional<DepChunk> get(lines_t beg, lines_t end){
        DepChunk chunk{};
        auto it=beg;
        for(;!isSentBeg(*it);++it) if(it==end) return {};
        assert(isSentBeg(*it));
        chunk.beg=it;
        for(;!isSentEnd(*it);++it) if(it==end) return {};
        assert(isSentEnd(*it));
        chunk.end=it;
        return chunk;
    }
    lines_t beg;
    lines_t end;
};

struct DepChunkParser{
    DepChunkParser(std::string filename)
    : lines{util::string::readlines(filename)}
    {}
    template<typename OP>
    void iter(OP const &op) const {
        auto beg = lines.cbegin();
        auto end = lines.cend();
        while(auto maybe_chunk = DepChunk::get(beg, end)){
            auto chunk = maybe_chunk.value();
            op(chunk);
            beg = chunk.end;
        }
    }

    std::vector<std::string> const lines;
};

void parse_batch_output_line(){
    {
        assert(isSentBeg("Sentence #1 (9 tokens):"));
        assert(!isSentBeg(""));
        assert(!isSentEnd("Sentence #1 (9 tokens):"));
        assert(isSentEnd(""));
    }
    {
        std::string line = "[Text=Dave CharacterOffsetBegin=0 CharacterOffsetEnd=4 PartOfSpeech=NNP]";
        WordToken test{line,0};
        assert(test.pos=="NNP");
        assert(test.self.word=="Dave");
        assert(test.self.idx==0);
        assert(test.offset.beg==0);
        assert(test.offset.end==4);
    }
    {
        std::string line = "[Text== CharacterOffsetBegin=0 CharacterOffsetEnd=4 PartOfSpeech=NNP]";
        WordToken test{line,0};
        assert(test.pos=="NNP");
        assert(test.self.word=="=");
        assert(test.self.idx==0);
        assert(test.offset.beg==0);
        assert(test.offset.end==4);
    }
    {
        std::string line = "appos(Aneckstein-2, Research-5)";
        DepToken test{line};
        assert(test.arclabel == "appos");
        assert(test.dependent.word == "Aneckstein");
        assert(test.dependent.idx  == 1);
        assert(test.governor.word == "Research");
        assert(test.governor.idx  == 4);
    }
    {
        std::string line = u8"dobj(A-B-2, ROOT-0)";
        DepToken test{line};
        assert(test.arclabel == "dobj");
        assert(test.dependent.word == "A-B");
        assert(test.dependent.idx  == 1);
        assert(test.governor.word == "ROOT");
        assert(test.governor.idx  == -1);
    }
    {
        std::string line = u8"punct(가나다-2, ,-6)";
        DepToken test{line};
        assert(test.arclabel == "punct");
        assert(test.dependent.word == "가나다");
        assert(test.dependent.idx  == 1);
        assert(test.governor.word == ",");
        assert(test.governor.idx  == 5);
    }

    std::string line = "appos(Aneckstein-2, Research-5)";
    gsl::cstring_span<> aa = gsl::ensure_z(line.data());
    auto it_label_end = std::find(aa.cbegin(), aa.cend(), '(');
    fmt::print(std::cerr, "{} \n",gsl::to_string(aa.subspan(0, it_label_end-aa.cbegin())));
}

void parse_batch_output(){
    DepChunkParser parse{"../rnn++/tests/data/batch.corenlp"};
    parse.iter([](auto const &chunk){
        for(auto it=chunk.beg; it!=chunk.end; ++it) fmt::print("{}\n", *it);
    });

}

}//namespace data::corenlp::test
}//namespace data::corenlp
}//namespace data

int process_rss_dump(int argc, char** argv){
    assert(argc>1);
    auto config = util::load_json(argv[1]);
    util::Timer timer;

    auto hashes = util::get_str(config,"row_hashes");
    auto json_dump_path = util::get_str(config,"corenlp_dumps");
    auto dataset_prefix = util::get_str(config,"dep_parsed_prefix");

    data::CoreNLPoutputParser dump_parser{config};
    auto json_dumps = util::string::readlines(json_dump_path);
    timer.here_then_reset(fmt::format("Begin to process {} JSON dump files. ",json_dumps.size()));
    data::parallel_load_jsons(json_dumps, dump_parser);
    timer.here_then_reset(fmt::format("Parsed {} files. ",dump_parser.chunks.size()));
    auto tokens = dump_parser.get(dataset_prefix);
    auto non_null_idxs = dump_parser.get_nonnull_idx();
    timer.here_then_reset("Parsing is finished. ");

    auto output_filename = util::VersionedName{util::get_str(config,"dep_parsed_store"),
                                               DepParsedTokens::major_version, 0};
    tokens.write_to_disk(output_filename.fullname);
    data::rss::write_column_indexes(config, hashes, json_dump_path, non_null_idxs);
    return 0;
}
int process_ygp_dump(int argc, char** argv){
    assert(argc>1);
    auto config = util::load_json(argv[1]);
    util::Timer timer;

    auto json_dump_path = util::get_str(config,"corenlp_dumps");
    auto dataset_prefix = util::get_str(config,"dep_parsed_prefix");

    data::CoreNLPoutputParser dump_parser{config};
    auto json_dumps = util::string::readlines(json_dump_path);
    timer.here_then_reset(fmt::format("Begin to process JSON dump files. "));
    data::parallel_load_jsons(json_dumps, dump_parser);
    timer.here_then_reset(fmt::format("Parsed {} files. ",dump_parser.chunks.size()));
    auto tokens = dump_parser.get(dataset_prefix);
//    auto tokens = dump_parser.serial_parse(json_dumps, prefix);
    auto non_null_idxs = dump_parser.get_nonnull_idx();
    timer.here_then_reset("Parsing is finished. ");

    auto output_filename = util::VersionedName{util::get_str(config,"dep_parsed_store"),
                                               DepParsedTokens::major_version, 0};
    tokens.write_to_disk(output_filename.fullname);
    std::vector<std::string> non_null_dumps;
    for(auto i : non_null_idxs) non_null_dumps.push_back(json_dumps[i]);
    data::ygp::write_column_indexes(config, non_null_dumps);
    auto country_output_name = util::VersionedName{util::get_str(config,"country_uids_dump"),
                                                   DepParsedTokens::major_version, 0};
    data::ygp::write_country_code(config);
    return 0;
}

void test_rss(int argc, char** argv){
    assert(argc > 1);
    auto config = util::load_json(argv[1]);
    data::rss::test::rss_corenlp_path();
    data::rss::test::IndexUIDs(config);
    data::rss::test::rss_indexing(config);
}
void test_ygp(int argc, char** argv) {
    assert(argc > 1);
    auto config = util::load_json(argv[1]);
    data::ygp::test::ygpdb_indexing(config);
    data::ygp::test::country_annotator(config);
    data::ygp::test::country_code(config);
}

void test_common(int argc, char** argv){
    assert(argc > 1);
    auto config = util::load_json(argv[1]);
    test::word_importance(config);
    test::unicode_conversion();
    test::persistent_vector_float();
    test::persistent_vector_WordUID();
    test::filesystem(config);


    data::corenlp::test::parse_batch_output_line();
    data::corenlp::test::parse_batch_output();

}
int main(int argc, char** argv){
    assert(argc>1);
    auto config = util::load_json(argv[1]);
//    test_ygp(argc, argv);
    test_rss(argc, argv);
    test_common(argc, argv);
    return 0;

//    parse_textfile(dump_files);
//    pruning_voca();
    process_rss_dump(argc, argv);
//    process_ygp_dump(argc,argv);
    //data::ygp::parse_psql(get_str(config,"column_uids_dump"));

    return 0;
//    pruning_voca();
//    convert_h5py_to_native();
//    write_WordUIDs("/home/jihuni/word2vec/ygp/words.uid", "test.Google.h5", "news.en.words", "news.en.uids");
//    write_WordUIDs("/home/jihuni/word2vec/ygp/words.uid", "s2010.h5", "s2010.words", "s2010.uids");
}
