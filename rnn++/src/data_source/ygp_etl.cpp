#include <vector>
#include <algorithm>
#include <cctype>
#include <fstream>

#include <set>
#include <utils/versioned_name.h>

#include "pqxx/pqxx"
#include "fmt/printf.h"

#include "data_source/corenlp_helper.h"
#include "data_source/ygp_db.h"
#include "data_source/ygp_etl.h"

#include "wordrep/word_uid.h"
#include "wordrep/word_prob.h"
#include "wordrep/voca_info.h"
#include "wordrep/dep_parsed.h"

#include "utils/profiling.h"
#include "utils/span.h"
#include "utils/string.h"
#include "utils/parallel.h"
#include "utils/base_types.h"
#include "utils/random.h"
#include "utils/persistent_vector.h"

#include "utils/flatbuffers/io.h"

using namespace wordrep;
namespace fb = util::io::fb;

namespace {

struct Chunks{
    using idx_t = std::pair<ChunkIndex,SentUID>;
    Chunks(std::string prefix)
            : sents_uid{fb::load_binary_file(fb::I64Binary{prefix+".sent_uid.i64v"}),  "sent_uid"},
              chunks_idx{fb::load_binary_file(fb::I64Binary{prefix+".chunk_idx.i64v"}),"chunk_idx"}
    {
        assert(sents_uid.size()==chunks_idx.size());
    }

    DPTokenIndex token_beg() const {return DPTokenIndex{0};}
    DPTokenIndex token_end() const {return DPTokenIndex::from_unsigned(sents_uid.size());}
    ChunkIndex chunk_idx(DPTokenIndex idx) const {return chunks_idx[idx.val];}
    SentUID sent_uid(DPTokenIndex idx) const {return sents_uid[idx.val];}
    idx_t at(DPTokenIndex idx) const {return {chunk_idx(idx),sent_uid(idx)};}

    DPTokenIndex next_sent_beg(DPTokenIndex idx) const {
        auto current_sent_idx = at(idx);
        auto end = token_end();
        auto it=idx;
        while(it<end) if(at(++it)!=current_sent_idx) break;
        return it;
    }
private:
    util::TypedPersistentVector<SentUID>      sents_uid;
    util::TypedPersistentVector<ChunkIndex>   chunks_idx;
};





}//nameless namespace

namespace data {
namespace ygp {


int parse_column(std::string table, std::string column, std::string index_col){
    CoreNLPwebclient corenlp_client{"../rnn++/scripts/corenlp.py"};
    try {
        pqxx::connection C{"dbname=C291145_gbi_test host=bill.uphere.he"};
        std::cerr << "Connected to " << C.dbname() << std::endl;
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
    } catch (const std::exception &e) {
        std::cerr << e.what() << std::endl;
        return 1;
    }
    return 0;
}

int dump_column(std::string table, std::string column, std::string index_col, std::string dump_path){
    try {
        pqxx::connection C{"dbname=C291145_gbi_test host=bill.uphere.he"};
        std::cerr << "Connected to " << C.dbname() << std::endl;
        pqxx::work W(C);


        auto query=fmt::format("SELECT {}, {} FROM {};", column, index_col, table);
        auto body= W.exec(query);
        W.commit();
        auto n = body.size();
        for(decltype(n)i = 0; i!=n; ++i){
            auto row = body[i];
            std::string raw_text = row[0].c_str();
            auto index = row[1];
            auto row_full_name=fmt::format("{}.{}.{}.{}", table, column, index_col, index);
            if(util::string::isspace(raw_text)) {
                fmt::print(std::cerr, "Is it empty?? : {} is empty.\n", row_full_name);
                continue;
            }
            if(!dump_path.empty()){
                auto dump_full_path=fmt::format("{}/{}", dump_path, row_full_name);
                util::string::write_whole(dump_full_path, raw_text);
            }
            std::cout << raw_text << std::endl;
        }
        std::cout << std::endl;
    } catch (const std::exception &e){
        std::cerr << e.what() << std::endl;
        return 1;
    }
    return 0;
}

void parse_psql(std::string cols_to_exports){
    YGPdb db{cols_to_exports};
    for(auto col_uid =db.beg(); col_uid!=db.end(); ++col_uid){
        auto table = db.table(col_uid);
        auto column = db.column(col_uid);
        auto index_col = db.index_col(col_uid);
        std::cerr<<fmt::format("Dumping : {:15} {:15} {:15}\n", table, column, index_col)<<std::endl;
        parse_column(table, column, index_col);
    }
}
void dump_psql(std::string cols_to_exports, std::string dump_path){
    YGPdb db{cols_to_exports};
    for(auto col_uid =db.beg(); col_uid!=db.end(); ++col_uid){
        auto table = db.table(col_uid);
        auto column = db.column(col_uid);
        auto index_col = db.index_col(col_uid);
        std::cerr<<fmt::format("Dumping : {:15} {:15} {:15}\n", table, column, index_col)<<std::endl;
        dump_column(table, column, index_col, dump_path);
    }
}


void write_country_code(util::json_t const &config, int minor_version) {
    namespace ygp = data::ygp;

    WordUIDindex wordUIDs{config["word_uids_dump"].get<std::string>()};

    auto output_filename = util::VersionedName{util::get_str(config,"dep_parsed_store"),
                                               DepParsedTokens::major_version, minor_version}.fullname;
    auto prefix = config["dep_parsed_prefix"].get<std::string>();
    auto cols_to_exports = config["column_uids_dump"].get<std::string>();

    ygp::CountryColumn table2country_code{};

    std::string ygp_prefix = util::get_str(config,"dep_parsed_prefix");
    std::string text_prefix = util::get_str(config,"dep_parsed_bins");
    RowUID row_uid{};
    YGPdb db{cols_to_exports};
    DBIndexer ygp_indexer{text_prefix};
    Chunks ygp_chunks{text_prefix};
    std::map<std::string, util::TypedPersistentVector<RowUID>> rows_by_country;
    std::map<std::string, util::TypedPersistentVector<SentUID>> sents_by_country;
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
        auto n = body.size();
        for (decltype(n) i = 0; i != n; ++i) {
            auto elm = body[i];
            RowIndex row_idx{std::stoi(elm[0].c_str())};
            if(ygp_indexer.is_empty(col_uid,row_idx)) continue;
            auto row_uid = ygp_indexer.row_uid(col_uid, row_idx);
            std::string country = elm[1].c_str();
            rows_by_country[country].push_back(row_uid);
            auto& tmp = sents_by_country[country];
            for(auto sent_uid : sents_in_row[row_uid]) tmp.push_back(sent_uid);
        }
    }

    auto country_output_name = util::VersionedName{util::get_str(config,"country_uids_dump"),
                                                   DepParsedTokens::major_version, minor_version}.fullname;
    std::ofstream country_list{country_output_name};
    for(auto x : rows_by_country) country_list << x.first << std::endl;
    for(auto x : rows_by_country)
        fb::to_file(util::serialize(x.second.get()), {x.first+".row_uid.i64v"});
    for(auto x : sents_by_country)
        fb::to_file(util::serialize(x.second.get()), {x.first+".sent_uid.i64v"});
    country_list.close();
}


void write_column_indexes(std::string output_prefix,
                          std::string cols_to_exports,
                          std::string prefix,
                          std::vector<std::string> corenlp_outputs){
    std::vector<ColumnUID> col_uids;
    std::vector<RowIndex> row_idxs;
    std::vector<RowUID> row_uids;

    RowUID row_uid{};
    YGPdb db{cols_to_exports};
    for(auto dumpfile_path : corenlp_outputs){
        RowDumpFilePath row{dumpfile_path};
        auto col_uid = db.col_uid(row.full_column_name());
        RowIndex row_idx{row.index};

        col_uids.push_back(col_uid);
        row_idxs.push_back(row_idx);
        row_uids.push_back(row_uid);

        ++row_uid;
    }

    util::io::fb::to_file(util::serialize(row_uids), {output_prefix + ".chunk2row.i64v"});
    util::io::fb::to_file(util::serialize(row_idxs), {output_prefix + ".chunk2row_idx.i64v"});
    util::io::fb::to_file(util::serialize(col_uids), {output_prefix + ".chunk2col.i64v"});
}


}//namespace data::ygp
}//namespace data
