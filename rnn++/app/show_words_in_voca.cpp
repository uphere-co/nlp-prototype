#include <iostream>

#include "wordrep/word_uid.h"

#include "utils/json.h"
#include "utils/persistent_vector.h"

void show_voca_words(util::json_t const& config){
    wordrep::WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};
    PersistentVector<token_t,token_t::val_t> voca{util::io::h5read(config["wordvec_store"]),
                                                  config["voca_name"]};
    for(auto uid : voca) std::cout<< wordUIDs[uid] <<std::endl;
}

int main(int /*argc*/, char** argv) {
    auto config = util::load_json(argv[1]);
    show_voca_words(config);
    return 0;
}