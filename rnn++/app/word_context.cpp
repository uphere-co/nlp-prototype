#include <iostream>

#include <fmt/printf.h>

#include "wordrep/wordvec_deriving.h"

#include "similarity/config.h"

#include "utils/json.h"
#include "utils/optional.h"
#include "utils/versioned_name.h"
#include "utils/parallel.h"
#include "utils/profiling.h"

int main(int argc, char** argv){
    assert(argc>1);
    auto config = util::load_json(argv[1]);
    auto output_h5store_name = argv[2];
    engine::SubmoduleFactory factory{{config}};
    util::Timer timer;

    wordrep::WordUIDindex wordUIDs = factory.word_uid_index();
    tbb::task_group g;
    std::optional<wordrep::IndexedTexts> m_texts = {};
    std::optional<std::vector<std::string>> m_words = {};
    std::optional<std::vector<wordrep::WordUID>> m_wuids = {};
    g.run([&m_texts, &config](){
        m_texts = wordrep::IndexedTexts{util::io::h5read(util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname),
                               config["dep_parsed_prefix"]};
    });
    g.run([&m_wuids,&wordUIDs](){
        auto words = util::string::readlines(std::move(std::cin));
        m_wuids = util::map(words, [&wordUIDs](auto word){return wordUIDs[word];});
    });
    g.wait();
    timer.here_then_reset("Load data.");
    auto texts = m_texts.value();
    auto words_for_new_voca = m_wuids.value();
    timer.here_then_reset("Construct data objects.");
    auto base_voca = factory.voca_info();
    timer.here_then_reset(fmt::format("Load and sort base voca of {} words.", base_voca.indexmap.size()));

    auto new_voca_words = wordrep::split_unseen_words(base_voca, words_for_new_voca);
    auto unseen_words_with_context = get_ngram_contexts(texts, new_voca_words.unseen_words);

    assert(unseen_words_with_context.size()==new_voca_words.unseen_words.size());
    timer.here_then_reset(fmt::format("Get contexts of {} unseen words; {} words are already known.",
                                      unseen_words_with_context.size(), new_voca_words.known_words.size()));
    auto conf = [&config](auto x){return util::get_str(config, x);};
    //TODO: update this
    wordrep::write_to_disk(base_voca, new_voca_words.known_words, unseen_words_with_context,
                           output_h5store_name, conf("w2vmodel_name"), conf("voca_name"));

    return 0;
}
