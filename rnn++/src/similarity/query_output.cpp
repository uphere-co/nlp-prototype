#include "similarity/query_output.h"

#include "data_source/db.h"

#include "utils/math.h"

namespace engine{

util::json_t to_json(std::vector<data::PerSentQueryResult> const &results){
    util::json_t answer{};
    answer["score"]=util::json_t::array();
    answer["result_sent_country"]= util::json_t::array();
    answer["result_table_name"]  = util::json_t::array();
    answer["result_column_name"] = util::json_t::array();
    answer["result_index_col_name"]=util::json_t::array();
    answer["highlight_offset"]   = util::json_t::array();
    answer["clip_offset"]        = util::json_t::array();
    answer["score_with_offset"]  = util::json_t::array();

    answer["result_sent_uid"]    = util::json_t::array();
    answer["result_row_uid"]     = util::json_t::array();
    answer["result_row_idx"]     = util::json_t::array();
    answer["result_column_uid"]  = util::json_t::array();
    answer["result_offset"]      = util::json_t::array();

    for(auto const &result : results){
        answer["score"].push_back(result.score);
        answer["result_sent_country"].push_back(result.country);
        data::build_db_info_field(answer, result);
        answer["result_table_name"].push_back(result.table_name);
        answer["result_column_name"].push_back(result.column_name);
        answer["result_index_col_name"].push_back(result.index_col_name);
        auto tmp2 = result.highlight_offset;
        answer["highlight_offset"].push_back({tmp2.beg, tmp2.end});
        auto tmp3 = result.clip_offset;
        answer["clip_offset"].push_back({tmp3.beg, tmp3.end});

        util::json_t score_with_offset{};
        for(auto elm :result.scores_with_offset) {
            score_with_offset.push_back({elm.score,
                                         elm.query_token.beg, elm.query_token.end,
                                         elm.matched_token.beg, elm.matched_token.end});
        }
        answer["score_with_offset"].push_back(score_with_offset);
    }
    return answer;
}

void annotate_input_info(util::json_t &answer, data::QuerySentInfo const &info){
    answer["input_offset"]={info.offset.beg,info.offset.end};
    answer["input_uid"] = info.sent_uid;
    answer["cutoffs"] = info.cutoffs;
    answer["words"] = info.words;
}

util::json_t to_json(std::vector<data::QueryResult> const &answers){
    util::json_t output = util::json_t::array();

    for(auto &answer : answers){
        auto answer_json = to_json(answer.results);
        answer_json["n_relevant_matches"] = answer.n_relevant_matches;
        annotate_input_info(answer_json, answer.query);
        auto cutoff = util::math::sum(answer_json["cutoffs"].get<std::vector<double>>());
        for(double x : answer_json["score"]) answer_json["is_highly_meet"].push_back(x>cutoff);
        output.push_back(answer_json);
    }
    return output;
}


}//namespace engine
