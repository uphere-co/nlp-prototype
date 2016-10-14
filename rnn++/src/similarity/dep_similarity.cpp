#include "similarity/dep_similarity.h"

DepSimilaritySearch::DepSimilaritySearch(json_t const &config)
: voca{config["wordvec_store"], config["voca_name"],
       config["w2vmodel_name"], config["w2v_float_t"]},
  tokens{H5file{H5name{config["dep_parsed_store"].get<std::string>()},
                hdf5::FileMode::read_exist}, config["dep_parsed_text"]},
  sents{tokens.SegmentSentences()},
  sents_plain{util::string::readlines(config["plain_text"])}
{}



DepSimilaritySearch::json_t DepSimilaritySearch::process_queries(json_t ask) const {
    nlohmann::json& sent_json = ask["sentences"][0];
    std::vector<double> cutoff = ask["cutoffs"][0];
    std::string query_str = ask["queries"][0];
    DepParsedQuery query{cutoff, sent_json, voca.indexmap};
    BoWVQuery2 similarity{query_str, cutoff, voca};

    json_t answer{};
    for(auto sent: sents){
        if( query.is_similar(sent, tokens, similarity)) {
            answer[query_str].push_back(sents_plain[sent.uid.val]);
        }
    }
    return answer;
}