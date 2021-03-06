#include <vector>
#include <algorithm>
#include <cctype>
#include <fstream>
#include <set>

#include <pqxx/pqxx>
#include <fmt/printf.h>

#include "similarity/query_engine.h"
#include "similarity/config.h"

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
    engine::SubmoduleFactory factory{{config}};
    WordUIDindex wordUIDs = factory.word_uid_index();
    POSUIDindex   posUIDs = factory.pos_uid_index();
    ArcLabelUIDindex arclabelUIDs = factory.arclabel_uid_index();
    WordImportance word_importance = factory.word_importance();

    auto ask = util::load_json("../rnn++/tests/data/query.unittest.inf_cutoff.corenlp");
    DepParsedTokens query_tokens{};
    query_tokens.append_corenlp_output(ask);
    query_tokens.build_sent_uid(SentUID{SentUID::val_t{0x80000000}});
    auto sents = query_tokens.IndexSentences();
    for(auto sent : sents){
        for(auto idx :sent){
            auto wuid = sent.dict->word_uid(idx);
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

void test_all(int argc, char** argv){
    assert(argc > 1);
    auto config = util::load_json(argv[1]);
    word_importance(config);
    unicode_conversion();
    persistent_vector_float();
    persistent_vector_WordUID();
    filesystem(config);
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

    auto cols_to_exports = util::get_str(config,"column_uids_dump");
    YGPdb db{cols_to_exports};
    assert(db.col_uid("regulation.regtitle.regid")==ColumnUID{3});
    assert(db.col_uid("reach_reports.content.report_id")==ColumnUID{0});
    assert(db.is_in("reach_reports.content.report_id"));
    assert(!db.is_in("dsafasfafsafa.content.asfasdf"));
}

void chunks() {
}

void country_annotator(util::json_t const &config) {
    Factory factory{{config}};
    CountryCodeAnnotator country_tagger = factory.country_code_annotator();
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


void test_all(int argc, char** argv) {
    assert(argc > 1);
    auto config = util::load_json(argv[1]);
    ygpdb_indexing(config);
    country_annotator(config);
}
}//namespace data::ygp::test
}//namespace data::ygp
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

void test_all(int argc, char** argv){
    assert(argc > 1);
    auto config = util::load_json(argv[1]);

    parse_batch_output_line();
    parse_batch_output();
}

}//namespace data::corenlp::test
}//namespace data::corenlp
}//namespace data

int process_ygp_dump(int argc, char** argv){
    assert(argc>3);
    auto config = util::load_json(argv[1]);
    util::Timer timer;

    auto json_dump_path   = argv[2];
    auto dataset_prefix   = argv[3];

    engine::SubmoduleFactory factory{{config}};
    auto voca = factory.voca_info();
    timer.here_then_reset("Load data.");

    data::CoreNLPoutputParser dump_parser;
    auto json_dumps = util::string::readlines(json_dump_path);
    timer.here_then_reset(fmt::format("Begin to process {} JSON dump files. ",json_dumps.size()));
    data::parallel_load_jsons(json_dumps, dump_parser);
    timer.here_then_reset(fmt::format("Parsed {} files. ",dump_parser.chunks.size()));
    auto tokens = dump_parser.get();
    auto non_null_idxs = dump_parser.get_nonnull_idx();
    timer.here_then_reset("Parsing is finished. ");

    tokens.build_voca_index(voca.indexmap);
    timer.here_then_reset("Built voca index.");

    tokens.to_file({dataset_prefix});
    timer.here_then_reset("Write to files.");

    std::vector<std::string> non_null_dumps;
    for(auto i : non_null_idxs) non_null_dumps.push_back(json_dumps[i]);
    data::ygp::write_column_indexes(dataset_prefix,
                                    util::get_str(config,"column_uids_dump"),
                                    non_null_dumps);
    timer.here_then_reset("Write indexes.");
    return 0;
}



void test_common(int argc, char** argv){
    assert(argc > 1);
    auto config = util::load_json(argv[1]);

    data::corenlp::test::parse_batch_output_line();
    data::corenlp::test::parse_batch_output();
}

int main(int argc, char** argv){
    assert(argc>1);
    auto config = util::load_json(argv[1]);
//    data::ygp::test::test_all(argc, argv);
//    test::test_all(argc,argv);
//    data::corenlp::test::test_all(argc,argv);
//    return 0;
    process_ygp_dump(argc,argv);
    return 0;
}
