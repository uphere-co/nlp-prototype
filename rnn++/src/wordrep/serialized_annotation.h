#pragma once

#include <memory>

#include "wordrep/file_formats.h"
#include "wordrep/simiarity_score.h"
#include "wordrep/io.h"

namespace wordrep{
struct SerializedAnnotation{
    struct Binary{
        std::string name;
    };
    using CIT = std::vector<wordrep::io::EntityCandidate>::const_iterator;
    struct CandidateIterator{
        static CIT next_entity(CIT it, CIT end) {
            auto idx = it->token_idx();
            return std::find_if_not(it, end, [idx](auto x){return x.token_idx()==idx;});
        }

        CandidateIterator(CIT it, CIT end) : it{it}, file_end{end} {}
        auto operator*( void ) const {
            std::vector<Scoring::AmbiguousEntity::Candidate> candidates;
            auto last_candi = next_entity(it,file_end);
            candidates.reserve(last_candi-it);
            for(auto candi=it;candi!=last_candi ; ++candi) {
                candidates.push_back({candi->wiki_uid(), candi->score()});
                assert(it->token_idx()==candi->token_idx());
            }
            return candidates;
        }
        void operator++(void) {
            it = next_entity(it,file_end);
        }
        DPTokenIndex index() const { return it->token_idx();}
        bool operator!=(CandidateIterator rhs ) const {return it != rhs.it;}
    private:
        CIT it;
        CIT file_end;
    };
    struct CandidateIteration{
        CandidateIterator begin() const {return {file_beg,file_end};}
        CandidateIterator end()   const {return {file_end,file_end};}

        CIT file_beg;
        CIT file_end;
    };

    CandidateIteration iter_ambu() const {
        return CandidateIteration{candidates.cbegin(), candidates.cend()};
    }

    void sort();
    void to_file(Binary file) const;


    std::vector<io::EntityCandidate> candidates;
    std::vector<io::TaggedToken> tagged_tokens;
    bool is_sorted = false;
};

std::unique_ptr<SerializedAnnotation> load_binary_file(SerializedAnnotation::Binary const& file);

//AnnotationData is a collection of SerializedAnnotation::Binary.
struct AnnotationData{
    static AnnotationData factory(AnnotatedTokenFile const& file);

    AnnotationData(){}
    AnnotationData(int n_block){
        for(int i=0; i!=n_block; ++i)
            blocks.push_back(std::make_unique<SerializedAnnotation>());
    }

    void to_file(AnnotatedTokenFile const& file) const;

    std::vector<std::unique_ptr<SerializedAnnotation>> blocks;
};

}//namespace wordrep;
