// automatically generated by the FlatBuffers compiler, do not modify


#ifndef FLATBUFFERS_GENERATED_TAGGEDSENTENCES_WORDREP_IO_H_
#define FLATBUFFERS_GENERATED_TAGGEDSENTENCES_WORDREP_IO_H_

#include "flatbuffers/flatbuffers.h"

namespace wordrep {
namespace io {

struct EntityCandidate;

struct TaggedToken;

struct TaggedSentences;

MANUALLY_ALIGNED_STRUCT(8) EntityCandidate FLATBUFFERS_FINAL_CLASS {
 private:
  int64_t token_idx_;
  int64_t wiki_uid_;
  double score_;

 public:
  EntityCandidate() {
    memset(this, 0, sizeof(EntityCandidate));
  }
  EntityCandidate(const EntityCandidate &_o) {
    memcpy(this, &_o, sizeof(EntityCandidate));
  }
  EntityCandidate(int64_t _token_idx, int64_t _wiki_uid, double _score)
      : token_idx_(flatbuffers::EndianScalar(_token_idx)),
        wiki_uid_(flatbuffers::EndianScalar(_wiki_uid)),
        score_(flatbuffers::EndianScalar(_score)) {
  }
  int64_t token_idx() const {
    return flatbuffers::EndianScalar(token_idx_);
  }
  int64_t wiki_uid() const {
    return flatbuffers::EndianScalar(wiki_uid_);
  }
  double score() const {
    return flatbuffers::EndianScalar(score_);
  }
};
STRUCT_END(EntityCandidate, 24);

MANUALLY_ALIGNED_STRUCT(8) TaggedToken FLATBUFFERS_FINAL_CLASS {
 private:
  int64_t sent_uid_;
  int64_t token_idx_;
  uint64_t token_len_;

 public:
  TaggedToken() {
    memset(this, 0, sizeof(TaggedToken));
  }
  TaggedToken(const TaggedToken &_o) {
    memcpy(this, &_o, sizeof(TaggedToken));
  }
  TaggedToken(int64_t _sent_uid, int64_t _token_idx, uint64_t _token_len)
      : sent_uid_(flatbuffers::EndianScalar(_sent_uid)),
        token_idx_(flatbuffers::EndianScalar(_token_idx)),
        token_len_(flatbuffers::EndianScalar(_token_len)) {
  }
  int64_t sent_uid() const {
    return flatbuffers::EndianScalar(sent_uid_);
  }
  int64_t token_idx() const {
    return flatbuffers::EndianScalar(token_idx_);
  }
  uint64_t token_len() const {
    return flatbuffers::EndianScalar(token_len_);
  }
};
STRUCT_END(TaggedToken, 24);

struct TaggedSentences FLATBUFFERS_FINAL_CLASS : private flatbuffers::Table {
  enum {
    VT_CANDIDATES = 4,
    VT_TAGGED_TOKENS = 6
  };
  const flatbuffers::Vector<const EntityCandidate *> *candidates() const {
    return GetPointer<const flatbuffers::Vector<const EntityCandidate *> *>(VT_CANDIDATES);
  }
  const flatbuffers::Vector<const TaggedToken *> *tagged_tokens() const {
    return GetPointer<const flatbuffers::Vector<const TaggedToken *> *>(VT_TAGGED_TOKENS);
  }
  bool Verify(flatbuffers::Verifier &verifier) const {
    return VerifyTableStart(verifier) &&
           VerifyField<flatbuffers::uoffset_t>(verifier, VT_CANDIDATES) &&
           verifier.Verify(candidates()) &&
           VerifyField<flatbuffers::uoffset_t>(verifier, VT_TAGGED_TOKENS) &&
           verifier.Verify(tagged_tokens()) &&
           verifier.EndTable();
  }
};

struct TaggedSentencesBuilder {
  flatbuffers::FlatBufferBuilder &fbb_;
  flatbuffers::uoffset_t start_;
  void add_candidates(flatbuffers::Offset<flatbuffers::Vector<const EntityCandidate *>> candidates) {
    fbb_.AddOffset(TaggedSentences::VT_CANDIDATES, candidates);
  }
  void add_tagged_tokens(flatbuffers::Offset<flatbuffers::Vector<const TaggedToken *>> tagged_tokens) {
    fbb_.AddOffset(TaggedSentences::VT_TAGGED_TOKENS, tagged_tokens);
  }
  TaggedSentencesBuilder(flatbuffers::FlatBufferBuilder &_fbb)
        : fbb_(_fbb) {
    start_ = fbb_.StartTable();
  }
  TaggedSentencesBuilder &operator=(const TaggedSentencesBuilder &);
  flatbuffers::Offset<TaggedSentences> Finish() {
    const auto end = fbb_.EndTable(start_, 2);
    auto o = flatbuffers::Offset<TaggedSentences>(end);
    return o;
  }
};

inline flatbuffers::Offset<TaggedSentences> CreateTaggedSentences(
    flatbuffers::FlatBufferBuilder &_fbb,
    flatbuffers::Offset<flatbuffers::Vector<const EntityCandidate *>> candidates = 0,
    flatbuffers::Offset<flatbuffers::Vector<const TaggedToken *>> tagged_tokens = 0) {
  TaggedSentencesBuilder builder_(_fbb);
  builder_.add_tagged_tokens(tagged_tokens);
  builder_.add_candidates(candidates);
  return builder_.Finish();
}

inline flatbuffers::Offset<TaggedSentences> CreateTaggedSentencesDirect(
    flatbuffers::FlatBufferBuilder &_fbb,
    const std::vector<const EntityCandidate *> *candidates = nullptr,
    const std::vector<const TaggedToken *> *tagged_tokens = nullptr) {
  return wordrep::io::CreateTaggedSentences(
      _fbb,
      candidates ? _fbb.CreateVector<const EntityCandidate *>(*candidates) : 0,
      tagged_tokens ? _fbb.CreateVector<const TaggedToken *>(*tagged_tokens) : 0);
}

inline const wordrep::io::TaggedSentences *GetTaggedSentences(const void *buf) {
  return flatbuffers::GetRoot<wordrep::io::TaggedSentences>(buf);
}

inline const char *TaggedSentencesIdentifier() {
  return "wfts";
}

inline bool TaggedSentencesBufferHasIdentifier(const void *buf) {
  return flatbuffers::BufferHasIdentifier(
      buf, TaggedSentencesIdentifier());
}

inline bool VerifyTaggedSentencesBuffer(
    flatbuffers::Verifier &verifier) {
  return verifier.VerifyBuffer<wordrep::io::TaggedSentences>(TaggedSentencesIdentifier());
}

inline const char *TaggedSentencesExtension() {
  return "wfts";
}

inline void FinishTaggedSentencesBuffer(
    flatbuffers::FlatBufferBuilder &fbb,
    flatbuffers::Offset<wordrep::io::TaggedSentences> root) {
  fbb.Finish(root, TaggedSentencesIdentifier());
}

}  // namespace io
}  // namespace wordrep

#endif  // FLATBUFFERS_GENERATED_TAGGEDSENTENCES_WORDREP_IO_H_
