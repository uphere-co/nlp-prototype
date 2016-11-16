#include <vector>
#include <algorithm>
#include <regex>
#include <cassert>

#include "utils/string.h"

int foo(){

}
//re.compile("Sentence #[0-9]+(.)*").match("Sentence #1 (9 tokens):").group()

int main(int /*argc*/, char** argv){
    auto header_line = "Sentence #1 (9 tokens):";
    auto token_line = "[Text=They CharacterOffsetBegin=0 CharacterOffsetEnd=4 PartOfSpeech=PRP]";
    auto arclabel_line = "root(ROOT-0, charged-5)";

    std::regex is_header("Sentence #[0-9]+(.)*");
    std::regex is_token("\\[Text=\\S+ CharacterOffsetBegin=\\d+ CharacterOffsetEnd=\\d+ PartOfSpeech=\\S+");// PartOfSpeech=(.)*CharacterOffsetEnd=d+ PartOfSpeech=S+]
    std::regex get_token("Sentence #[0-9]+(.)*");
    assert(std::regex_match(header_line, is_header));
    assert(~std::regex_match(token_line, is_header));
    assert(~std::regex_match(arclabel_line, is_header));
    assert(~std::regex_match(header_line, is_token));
    assert(std::regex_match(token_line, is_token));
    assert(~std::regex_match(arclabel_line, is_token));
    //bool =

    //std::regex_match(fname, base_match, base_regex))
//    if (base_match.size() == 2) {
//        std::ssub_match base_sub_match = base_match[1];
    return 0;
}

