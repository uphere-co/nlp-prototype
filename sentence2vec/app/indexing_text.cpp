#include <limits>

#include "dataset.h"
#include "parser/voca.h"

#include "utils/json.h"
#include "utils/profiling.h"
#include "utils/parallel.h"
#include "utils/hdf5.h"

constexpr auto sep = -1;//  std::numeric_limits<idx_t>::max();

int main(int /*argc*/, char** argv){
    using namespace util;
    using namespace util::io;

    using idx_t = int32_t;// rnn::simple_model::VocaInfo::voca_idx_map_t::idx_t;

    Timer timer{};
    tbb::task_group g;

    auto config = load_json(argv[1]);
    auto voca = rnn::wordrep::load_voca(config["wordvec_store"], config["voca_name"]);
    auto word2idx = voca.indexing();

    timer.here_then_reset("Load voca info.");
    auto input_name = argv[2];
    auto dataset=rnn::TokenizedSentences{input_name};
    timer.here_then_reset("Read raw text.");

    auto &lines = dataset.val;
    auto n = lines.size();
    std::vector<std::vector<idx_t>> indexed(n);

    tbb::parallel_for(decltype(n){0}, n, [&](auto i) {
        auto sent=lines[i];
        auto idxs = word2idx.getIndex(sent);
        std::copy(idxs.cbegin(), idxs.cend(), std::back_inserter(indexed[i]));
    });
    timer.here_then_reset("Indexing finished.");

    std::vector<idx_t> serialized;
    for(auto const &idxs : indexed){
        std::copy(idxs.cbegin(), idxs.cend(), std::back_inserter(serialized));
        serialized.push_back(sep);
    }

//    H5file h5store{H5name{"indexed_text.h5"}, hdf5::FileMode::replace};
//    h5store.writeRawData(H5name{input_name}, serialized);
    H5file h5store{H5name{"indexed_text.h5"}, hdf5::FileMode::rw_exist};
    h5store.writeRawData(H5name{input_name}, serialized);
    timer.here_then_reset("Wrote to HDF5.");
    return 0;
}
