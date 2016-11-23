#include <vector>
#include <algorithm>
#include <cctype>
#include <fstream>

#include <set>

#include "pqxx/pqxx"
#include "fmt/printf.h"

#include "data_source/corenlp_helper.h"
#include "data_source/ygp_db.h"

#include "wordrep/word_uid.h"
#include "wordrep/word_prob.h"
#include "wordrep/voca_info.h"
#include "wordrep/dep_parsed.h"

#include "utils/hdf5.h"
#include "utils/profiling.h"
#include "utils/span.h"
#include "utils/string.h"
#include "utils/parallel.h"
#include "utils/base_types.h"
#include "utils/random.h"


using namespace wordrep;
using namespace util::io;
using namespace ::ygp;

namespace {

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
        auto current_sent_idx = at(idx);
        auto end = token_end();
        auto it=idx;
        while(it<end) if(at(++it)!=current_sent_idx) break;
        return it;
    }
private:
    std::vector<SentUID>      sents_uid;
    std::vector<ChunkIndex>   chunks_idx;
};


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



}//nameless namespace

namespace data {
namespace ygp {


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
    ::ygp::YGPdb db{cols_to_exports};
    for(auto col_uid =db.beg(); col_uid!=db.end(); ++col_uid){
        auto table = db.table(col_uid);
        auto column = db.column(col_uid);
        auto index_col = db.index_col(col_uid);
        std::cerr<<fmt::format("Dumping : {:15} {:15} {:15}\n", table, column, index_col)<<std::endl;
        dump_column(table, column, index_col);
    }
}


void write_country_code(util::json_t const &config,
                        std::string cols_to_exports) {
    using namespace ::ygp;
    namespace ygp = data::ygp;

    VocaInfo voca{config["wordvec_store"], config["voca_name"],
                  config["w2vmodel_name"], config["w2v_float_t"]};
    WordUIDindex wordUIDs{config["word_uids_dump"].get<std::string>()};

    auto output_filename = config["dep_parsed_store"].get<std::string>();
    auto prefix = config["dep_parsed_prefix"].get<std::string>();

    ygp::CountryColumn table2country_code{};

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
        auto n = body.size();
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

void parse_json_dumps(nlohmann::json const &config,
                      std::string cols_to_exports, int64_t n_max=-1){
    VocaInfo voca{config["wordvec_store"], config["voca_name"],
                  config["w2vmodel_name"], config["w2v_float_t"]};
    WordUIDindex wordUIDs{config["word_uids_dump"].get<std::string>()};
    POSUIDindex posUIDs{config["pos_uids_dump"].get<std::string>()};
    ArcLabelUIDindex arclabelUIDs{config["arclabel_uids_dump"].get<std::string>()};

    auto output_filename = config["dep_parsed_store"].get<std::string>();
    auto prefix = config["dep_parsed_prefix"].get<std::string>();

    std::vector<ColumnUID> col_uids;
    std::vector<RowIndex> row_idxs;
    std::vector<RowUID> row_uids;
    DepParsedTokens tokens{};
    ColumnUID col_uid{};
    RowUID row_uid{};

    YGPdb db{cols_to_exports};
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

            RowIndex row_idx{RowIndex::val_t{index}};
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


}//namespace data::ygp
}//namespace data
