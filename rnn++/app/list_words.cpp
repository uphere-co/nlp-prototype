#include <fmt/printf.h>

#include "wordrep/word_uid.h"

#include "utils/hdf5.h"
#include "utils/persistent_vector.h"
#include "utils/json.h"
#include "utils/versioned_name.h"

using wordrep::WordUID;

int main(int argc, char** argv){
    std::string voca_store, voca_name, words_file;
    if(argc==2){
        auto config = util::load_json(argv[1]);
        voca_store = util::get_str(config,"wordvec_store");
        voca_name  = util::get_str(config,"voca_name");
        words_file = util::get_str(config,"word_uids_dump");
    } else {
        assert(argc > 3);
        voca_store = argv[1];
        voca_name = argv[2];
        words_file = argv[3];
    }
    wordrep::WordUIDindex wordUIDs(words_file);
    auto wordvec_file=util::io::h5read(voca_store);
    util::TypedPersistentVector<WordUID> words {wordvec_file, voca_name};

    for(auto word : words)
        fmt::print("{}\n", wordUIDs[word]);
    return 0;
}
