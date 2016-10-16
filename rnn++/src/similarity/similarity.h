#pragma once

#include "parser/parser.h"
#include "parser/wordvec.h"

#include "utils/json.h"
#include "utils/span.h"

#include <vector>

struct SimilaritySearch{
    using json_t = nlohmann::json;
    using voca_info_t = rnn::simple_model::VocaInfo;
    using param_t     = rnn::simple_model::Param;
    SimilaritySearch(json_t const &config)
    :   sent_vecs{rnn::wordrep::load_voca_vecs<param_t::dim>(config["phrase_store"], config["phrase_vec"],
                                                             util::datatype_from_string(config["w2v_float_t"]))},
        phrase_voca{rnn::wordrep::load_voca(config["phrase_store"], config["phrase_word"])},
        param{rnn::simple_model::load_param(config["rnn_param_store"], config["rnn_param_uid"],
                                            util::datatype_from_string(config["param_float_t"]))},
        rnn{config["wordvec_store"], config["voca_name"], config["w2vmodel_name"],
            util::datatype_from_string(config["w2v_float_t"])}
    {}

    json_t static parse(const char *query) {return json_t::parse(query);}

    json_t process_queries(json_t ask) const;

    voca_info_t::voca_vecs_t sent_vecs;
    voca_info_t::voca_t  phrase_voca;
    param_t param;
    voca_info_t rnn;
};


constexpr auto sep = -1;//  std::numeric_limits<idx_t>::max();

std::vector<int32_t> load_data(std::string datset_name);

struct IndexedSentences{
    IndexedSentences(std::string filename)
    : val{load_data(filename)} {
        auto beg=val.cbegin();
        auto end=std::find(beg, val.cend(), sep);
        while(end!=val.cend()){
            sents.push_back(util::as_span(&(*beg),&(*end)));
            beg=end+1;
            end=std::find(beg, val.cend(), sep);
        }
    }
    IndexedSentences(const char* filename) : IndexedSentences(std::string{filename}) {}

    std::vector<int32_t> val;
    std::vector<util::span_dyn<const int32_t>> sents;
};

struct BoWVSimilaritySearch{
    using json_t = nlohmann::json;
    using voca_info_t = rnn::simple_model::VocaInfo;
    BoWVSimilaritySearch(json_t const &config)
    : rnn{config["wordvec_store"], config["voca_name"], config["w2vmodel_name"],
          util::datatype_from_string(config["w2v_float_t"])},
      text{util::get_string_val(config,"textset")}, lines{util::get_string_val(config,"textset")}
    {}
    json_t process_queries(json_t ask) const;

    voca_info_t rnn;
    IndexedSentences text;
    rnn::ParsedSentences lines;
};

struct SentUIndex{
    SentUIndex(std::ptrdiff_t val) : val{val}{}
    int64_t val;
};

//TODO:rename it to WVID or so.
struct WordUIndex{
    WordUIndex(std::ptrdiff_t val) : val{val}{}
    int64_t val;
};
struct SentPosition{int64_t val;};
struct Sentence{
    Sentence(SentUIndex uid, WordUIndex beg, WordUIndex end)
    : uid{uid}, beg{beg}, end{end} {}
    SentUIndex uid;
    WordUIndex beg;
    WordUIndex end;
};


struct ParsedWord;
struct ParsedWordIdx{
    ParsedWordIdx(ParsedWord const &words, rnn::wordrep::VocaIndexMap const &word2idx);
    ParsedWordIdx(util::io::H5file const &file, std::string prefix);

    void write_to_disk(std::string filename, std::string prefix) const;
    std::vector<Sentence> SegmentSentences() const;

    std::vector<int64_t>     sent_idx;
    std::vector<int64_t>     word;
    std::vector<int64_t>     word_pidx;
    std::vector<int64_t>     head_word;
    std::vector<int64_t>     head_pidx;
    std::vector<char>        arc_label_raw;
    std::vector<const char*> arc_label;
};

struct DepParseSearch{
    using json_t = nlohmann::json;
    using voca_info_t = rnn::simple_model::VocaInfo;
    DepParseSearch(json_t const &config);
    json_t process_queries(json_t ask) const;
    voca_info_t rnn;
    ParsedWordIdx tokens;
    std::vector<Sentence> sents;
    std::vector<std::string> sents_plain;
};

void convert_h5py_to_native();
