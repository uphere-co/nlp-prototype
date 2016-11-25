#pragma once

#include "utils/json.h"

namespace data {

class CoreNLPjson {
public:
    CoreNLPjson(std::string filename)
            : val(util::load_json(filename)) {}

    template<typename OP>
    void iter_tokens(OP const &op) const {
        for (auto const &sent_json : val["sentences"]) {
            for (auto const &token : sent_json["tokens"]) op(token);
        }
    }

    template<typename OP>
    void iter_basic_dep_tokens(OP const &op) const {
        for (auto const &sent_json : val["sentences"]) {
            for (auto const &token2 : sent_json["basicDependencies"]) op(token2);
        }
    }
    void add_uid(size_t uid_) { uid=uid_;}

//private:
    size_t uid;
    util::json_t val;
};

}//namespace data
