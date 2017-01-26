#include "similarity/word_importance.h"
#include "similarity/config.h"

#include <map>
#include <cmath>

#include <fmt/printf.h>

#include "wordrep/word_uid.h"

#include "data_source/db.h"
#include "similarity/config.h"

#include "utils/algorithm.h"
#include "utils/hdf5.h"
#include "utils/persistent_vector.h"
#include "utils/versioned_name.h"
#include "utils/profiling.h"


using wordrep::WordUID;
using wordrep::ChunkIndex;

namespace engine{

void accum_word_importance_count(util::json_t const& config,
                                 std::vector<ColumnPair> const& column_pairs,
                                 std::map<WordUID,CaseCount>& cases,
                                 int64_t& n_case){
    using util::io::h5read;
    fmt::print(std::cerr, "Read {}\n",
               util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname);

    util::Timer timer;
    auto file = h5read(util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname);
    std::string prefix = config["dep_parsed_prefix"];
    util::TypedPersistentVector<ChunkIndex> chunks_idx{file,prefix+".chunk_idx"};
    util::TypedPersistentVector<WordUID>    words_uid {file,prefix+".word_uid"};

    SubmoduleFactory factory{{config}};
    wordrep::WordUIDindex wordUIDs   = factory.word_uid_index();
    data::DBIndexer indexer = factory.db_indexer();
    timer.here_then_reset("Data loaded.");

    std::map<ChunkIndex, std::vector<WordUID>> chunks;

    auto beg_idx = chunks_idx.cbegin();
    auto end_idx = chunks_idx.end();
    auto beg_word = words_uid.cbegin();
    auto chunk_beg = beg_idx;
    while(chunk_beg!=end_idx){
        auto chunk_end = std::find_if_not(chunk_beg, end_idx, [chunk_beg](auto x){return *chunk_beg==x;});
        for(auto it=beg_word+std::distance(beg_idx, chunk_beg);
            it!=beg_word+std::distance(beg_idx, chunk_end); ++it) chunks[*chunk_beg].push_back(*it);
        chunk_beg=chunk_end;
    }

    using data::ColumnUID;
    using data::RowIndex;
    std::map<ColumnUID, std::map<RowIndex,ChunkIndex>> index_map;
    for(auto const& chunk : chunks){
        auto chunk_idx  = chunk.first;
        auto column_uid = indexer.column_uid(chunk_idx);
        auto row_idx    = indexer.row_idx(chunk_idx);
        index_map[column_uid][row_idx]=chunk_idx;
    }
    for(auto elm : index_map){
        fmt::print(std::cerr, "{} : {} chunks.\n", elm.first, elm.second.size());
    }

    for(auto pair : column_pairs){
        auto rows_full = index_map[pair.full];
        auto rows_summary = index_map[pair.summary];
        assert(index_map.find(pair.full)!=index_map.end());
        assert(index_map.find(pair.summary)!=index_map.end());

        for(auto elm : rows_summary){
            auto row_idx = elm.first;
            if(rows_full.find(row_idx)==rows_full.end()) continue;
            ++n_case;
            auto summary_chunk = rows_summary[row_idx];
            auto full_chunk = rows_full[row_idx];
            assert(rows_summary.find(row_idx)!=rows_summary.end());
            assert(rows_full.find(row_idx)!=rows_full.end());

            auto words_in_summary = util::unique_values(chunks[summary_chunk]);
            auto words_in_full    = util::unique_values(chunks[full_chunk]);

            for(auto idx : words_in_summary){
                cases[idx].summary +=1;
                if(util::isin(words_in_full, idx)) cases[idx].both +=1;
            }
            for(auto idx : words_in_full) cases[idx].full +=1;

            if(false){
                if(n_case>10) continue;
                fmt::print("RowIndex {}:\n", row_idx);
                fmt::print("summary: {}\n", summary_chunk);
                assert(chunks.find(summary_chunk)!=chunks.end());
                assert(chunks.find(full_chunk)!=chunks.end());
                for(auto idx : chunks[summary_chunk]) fmt::print("{} ", wordUIDs[idx]);
                fmt::print("\n");
                fmt::print("full: {}\n", full_chunk);
                for(auto idx : chunks[full_chunk]) fmt::print("{} ", wordUIDs[idx]);
                fmt::print("\n");
            }
        }
    }
}
void build_word_importance(){
    auto ygp_config = util::load_json("config.ygp.json");
    auto rss_config = util::load_json("config.rss.json");

    engine::SubmoduleFactory factory{{rss_config}};
    wordrep::WordUIDindex wordUIDs = factory.word_uid_index();

    int64_t n_case=0;
    std::map<WordUID,CaseCount> cases;

    std::vector<ColumnPair> ygp_column_pairs = {{1,0},{2,1},{2,0}, {3,4},{3,5}, {4,5}};//for YGP
    accum_word_importance_count(ygp_config, ygp_column_pairs, cases, n_case);
    std::vector<ColumnPair> rss_column_pairs = {{1,2}};//for RSS. 0 is title and all capital.
//    accum_word_importance_count(rss_config, rss_column_pairs, cases, n_case);

    auto ratio_per_uids = util::to_pairs(cases);
    auto norm_factor = 1.0/n_case;
    for(auto& elm : ratio_per_uids) {
        auto& x = elm.second;
        if(x.summary<5 || x.full<5)
            x.ratio=0.0;
        else
            x.ratio = x.both/(norm_factor*x.summary*x.full);
    }
    //util::sort(ratio_per_uids, [](auto x, auto y){return x.second.ratio>y.second.ratio;});
    auto score = [](auto& x){
        if(x.second.ratio<0.1 || !x.second.both) return 0.0;
        auto f_ratio = std::log(x.second.ratio);
//        auto f_count = std::log(x.second.both);
        auto f_count = std::pow(x.second.both,0.3);
        auto tmp = f_ratio * f_count;
//        return tmp>0.0? std::log(tmp):-10.0;
        return tmp;
    };
    //util::sort(ratio_per_uids, [](auto x, auto y){return x.second.ratio>y.second.ratio;});
    util::sort(ratio_per_uids, [score](auto x, auto y){return score(x)>score(y);});
    for(auto x : ratio_per_uids){
        fmt::print("{:<15}\t{:<5}\t{}\t{}\t{}\t{}\t{} {}\n", wordUIDs[x.first], x.second.ratio,
                   x.second.full, x.second.summary, x.second.both, score(x),
                   std::log(x.second.ratio), std::log(x.second.both));
    }
    return ;
    util::TypedPersistentVector<WordUID> uids{"prob.word_uid", util::map(ratio_per_uids, [](auto x){return x.first;})};
    util::PersistentVector<float,CaseCount::float_t> ratios{"prob.ratio", util::map(ratio_per_uids, [](auto x){return x.second.ratio;})};

    auto output = util::io::h5replace("prob.h5");
    uids.write(output);
    ratios.write(output);
    return;
}

}//namespace engine
