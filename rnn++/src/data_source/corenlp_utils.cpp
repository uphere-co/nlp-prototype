#include "data_source/corenlp_utils.h"

#include "data_source/corenlp.h"

#include "wordrep/word_uid.h"
#include "wordrep/word_prob.h"
#include "wordrep/voca_info.h"
#include "wordrep/dep_parsed.h"

#include "utils/parallel.h"
#include "utils/string.h"

namespace data {

jsons_t parallel_load_jsons(std::string file_names){
    auto files = util::string::readlines(file_names);
    auto n = files.size();
    jsons_t jsons;
    tbb::parallel_for(decltype(n){0}, n, [&](auto i) {
        auto const &file = files[i];
        data::CoreNLPjson json{file};
        json.add_uid(i);
        jsons.push_back(std::move(json));
    });
    std::sort(jsons.begin(), jsons.end(), [](auto &x, auto &y){return x.uid<y.uid;});
//    for(auto file : files) jsons.push_back(data::CoreNLPjson{file});
    assert(jsons.front().uid==0);
    assert(jsons.back().uid ==n-1);
    return jsons;
}

StrCount parallel_word_count(jsons_t const &jsons){
    StrCount wc;
    for(auto const &json : jsons){
        json.iter_tokens([&](auto const &token){
            auto word = token["word"].template get<std::string>();
            ++wc.val[word];
        });
    }
    return wc;
}


void parse_json_dumps(util::json_t const &config,
                      jsons_t const &jsons){
    using namespace wordrep;
    VocaInfo voca{config["wordvec_store"], config["voca_name"],
                  config["w2vmodel_name"], config["w2v_float_t"]};
    WordUIDindex wordUIDs{config["word_uids_dump"].get<std::string>()};
    POSUIDindex posUIDs{config["pos_uids_dump"].get<std::string>()};
    ArcLabelUIDindex arclabelUIDs{config["arclabel_uids_dump"].get<std::string>()};

    auto output_filename = config["dep_parsed_store"].get<std::string>();
    auto prefix = config["dep_parsed_prefix"].get<std::string>();

    DepParsedTokens tokens{};
    for(auto parsed_json : jsons){
        tokens.append_corenlp_output(wordUIDs, posUIDs, arclabelUIDs, parsed_json.val);
    }
    tokens.build_sent_uid(SentUID{SentUID::val_t{0}});
    tokens.build_voca_index(voca.indexmap);
    tokens.write_to_disk(output_filename, prefix);

    wordUIDs.write_to_disk(config["word_uids_dump"].get<std::string>());
}


}//namespace data
