#include <fmt/printf.h>

#include "data_source/rss.h"
#include "data_source/corenlp_helper.h"
#include "data_source/corenlp_utils.h"

#include "utils/hdf5.h"
#include "utils/profiling.h"
#include "utils/string.h"
#include "utils/persistent_vector.h"
#include "utils/versioned_name.h"

int process_rss_dump(int argc, char** argv){
    assert(argc>1);
    auto config = util::load_json(argv[1]);
    util::Timer timer;

    auto hashes           = argv[2];
    auto json_dump_path   = argv[3];
    auto dataset_prefix   = util::get_str(config,"dep_parsed_prefix");
    auto dep_parsed_store = util::get_str(config,"dep_parsed_store");

    data::CoreNLPoutputParser dump_parser{config};
    auto json_dumps = util::string::readlines(json_dump_path);
    timer.here_then_reset(fmt::format("Begin to process {} JSON dump files. ",json_dumps.size()));
    data::parallel_load_jsons(json_dumps, dump_parser);
    timer.here_then_reset(fmt::format("Parsed {} files. ",dump_parser.chunks.size()));
    auto tokens = dump_parser.get(dataset_prefix);
    auto non_null_idxs = dump_parser.get_nonnull_idx();
    timer.here_then_reset("Parsing is finished. ");

    auto output_filename = util::VersionedName{dep_parsed_store,
                                               wordrep::DepParsedTokens::major_version, 0};
    tokens.write_to_disk(output_filename.fullname);
    data::rss::write_column_indexes(config, hashes, json_dump_path, non_null_idxs);
    return 0;
}

int main(int argc, char** argv){
    process_rss_dump(argc, argv);

    return 0;
}
