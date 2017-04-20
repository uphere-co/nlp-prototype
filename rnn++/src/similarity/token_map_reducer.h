#pragma once

#include "utils/algorithm.h"

namespace engine{

template<typename KEY, typename VALUE>
struct MatchPerKey{
    using Key   = KEY;
    using Value = VALUE;
    friend bool operator==(MatchPerKey const& x, MatchPerKey const& y){
        return x.key==y.key;
    }
    friend bool operator<(MatchPerKey const& x, MatchPerKey const& y){
        //DESCENDING ordering for VALUE if KEY is same; since ordering of VALUE is usually based on its score.
        if(x.key==y.key) return x.val>y.val;
        return x.key<y.key;
    }

    KEY key;
    VALUE val;
};

struct MatchedToken{
    friend bool operator==(MatchedToken const& x, MatchedToken const& y){
        return x.score==y.score;
    }
    friend bool operator>(MatchedToken const& x, MatchedToken const& y){
        return x.score>y.score;
    }

    wordrep::ConsecutiveTokens query;
    wordrep::ConsecutiveTokens matched;
    double score;
};

using EntityMatchPerSent = MatchPerKey<wordrep::SentUID,LookupEntityCandidate::Index>;
using WordMatchPerSent   = MatchPerKey<wordrep::SentUID,wordrep::DPTokenIndex>;
using MatchedTokenPerSent= MatchPerKey<wordrep::SentUID,MatchedToken>;

struct MatchedTokenReducer{
    using Key = MatchedTokenPerSent::Key;
    struct Value{
        double score;
        std::vector<MatchedTokenPerSent::Value> tokens;
    };

    static MatchedTokenReducer intersection(std::vector<std::vector<MatchedTokenPerSent>>&& xs){
        MatchedTokenReducer commons;
        if(xs.empty()) return commons;

        for(auto& token : xs.back())
            commons.vals[token.key]={token.val.score, {token.val}};
        xs.pop_back();

        for(auto& tokens : xs){
            commons.drop_complement(tokens);
            for(auto& token : tokens) commons.accum_if(token);
        }
        return commons;
    }

    std::vector<std::pair<Key,Value>> top_n_results(size_t n) const {
        auto results = util::to_pairs(vals);
        auto beg = results.begin();
        auto end = results.end();
        std::partial_sort(beg, beg+n, end, [](auto& x, auto& y){return x.second.score > y.second.score;});
        results.erase(beg+n, end);
        return results;
    }

    void score_filtering(double cutoff=0.0){
        for(auto it = vals.begin(); it != vals.end(); ){
            if(it->second.score > cutoff) {
                ++it;
                continue;
            }
            it = vals.erase(it);
        }
    }
    bool accum_if(MatchedTokenPerSent const &token){
        if(!has_key(token.key)) return false;
        vals[token.key].score += token.val.score;
        vals[token.key].tokens.push_back(token.val);
        return true;
    }
    void drop_complement(std::vector<MatchedTokenPerSent> const& tokens) {
        auto keys = util::map(tokens, [](auto& x){return x.key;});
        util::sort(keys);
        for(auto it = vals.begin(); it != vals.end(); ){
            if(util::binary_find(keys, it->first)) {
                ++it;
                continue;
            }
            it = vals.erase(it);
        }
    }
    std::map<Key,Value> vals;
private:
    bool has_key(Key key) const {
        if(vals.find(key)==vals.cend()) return false;
        return true;
    }
};

}//namespace engine
