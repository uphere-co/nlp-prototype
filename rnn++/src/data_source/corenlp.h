#pragma once

#include "utils/json.h"

namespace data {

class CoreNLPjson {
public:
    CoreNLPjson(std::string filename)
            : val{util::load_json(filename)}
    {}
    CoreNLPjson(util::json_t const &json)
            : val{json}
    {}

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
    template<typename OP,typename OP_TOKEN,typename OP_DEP, typename OP2>
    void iter_sent(OP const &op_pre_sent, OP_TOKEN const &op_token,
                   OP_DEP const &op_dep, OP2 const &op_post_sent) const {
        for (auto const &sent_json : val["sentences"]) {
            op_pre_sent(sent_json);
            for (auto const &token : sent_json["tokens"]) op_token(token);
            for (auto const &token2 : sent_json["basicDependencies"]) op_dep(token2);
            op_post_sent(sent_json);
        }
    }
    void add_uid(size_t uid_) { uid=uid_;}

//private:
    size_t uid;
    util::json_t val;
};

}//namespace data
