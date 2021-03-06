// automatically generated by the FlatBuffers compiler, do not modify


#ifndef FLATBUFFERS_GENERATED_SORTEDENTITY_WORDREP_WIKI_IO_H_
#define FLATBUFFERS_GENERATED_SORTEDENTITY_WORDREP_WIKI_IO_H_

#include "flatbuffers/flatbuffers.h"

namespace wordrep {
namespace wiki {
namespace io {

struct Entity;

struct SortedEntities;

MANUALLY_ALIGNED_STRUCT(8) Entity FLATBUFFERS_FINAL_CLASS {
 private:
  int64_t uid_;
  uint32_t name_beg_;
  uint32_t name_end_;

 public:
  Entity() {
    memset(this, 0, sizeof(Entity));
  }
  Entity(const Entity &_o) {
    memcpy(this, &_o, sizeof(Entity));
  }
  Entity(int64_t _uid, uint32_t _name_beg, uint32_t _name_end)
      : uid_(flatbuffers::EndianScalar(_uid)),
        name_beg_(flatbuffers::EndianScalar(_name_beg)),
        name_end_(flatbuffers::EndianScalar(_name_end)) {
  }
  int64_t uid() const {
    return flatbuffers::EndianScalar(uid_);
  }
  uint32_t name_beg() const {
    return flatbuffers::EndianScalar(name_beg_);
  }
  uint32_t name_end() const {
    return flatbuffers::EndianScalar(name_end_);
  }
};
STRUCT_END(Entity, 16);

struct SortedEntities FLATBUFFERS_FINAL_CLASS : private flatbuffers::Table {
  enum {
    VT_ENTITIES = 4,
    VT_NAMES = 6
  };
  const flatbuffers::Vector<const Entity *> *entities() const {
    return GetPointer<const flatbuffers::Vector<const Entity *> *>(VT_ENTITIES);
  }
  const flatbuffers::Vector<int64_t> *names() const {
    return GetPointer<const flatbuffers::Vector<int64_t> *>(VT_NAMES);
  }
  bool Verify(flatbuffers::Verifier &verifier) const {
    return VerifyTableStart(verifier) &&
           VerifyField<flatbuffers::uoffset_t>(verifier, VT_ENTITIES) &&
           verifier.Verify(entities()) &&
           VerifyField<flatbuffers::uoffset_t>(verifier, VT_NAMES) &&
           verifier.Verify(names()) &&
           verifier.EndTable();
  }
};

struct SortedEntitiesBuilder {
  flatbuffers::FlatBufferBuilder &fbb_;
  flatbuffers::uoffset_t start_;
  void add_entities(flatbuffers::Offset<flatbuffers::Vector<const Entity *>> entities) {
    fbb_.AddOffset(SortedEntities::VT_ENTITIES, entities);
  }
  void add_names(flatbuffers::Offset<flatbuffers::Vector<int64_t>> names) {
    fbb_.AddOffset(SortedEntities::VT_NAMES, names);
  }
  SortedEntitiesBuilder(flatbuffers::FlatBufferBuilder &_fbb)
        : fbb_(_fbb) {
    start_ = fbb_.StartTable();
  }
  SortedEntitiesBuilder &operator=(const SortedEntitiesBuilder &);
  flatbuffers::Offset<SortedEntities> Finish() {
    const auto end = fbb_.EndTable(start_, 2);
    auto o = flatbuffers::Offset<SortedEntities>(end);
    return o;
  }
};

inline flatbuffers::Offset<SortedEntities> CreateSortedEntities(
    flatbuffers::FlatBufferBuilder &_fbb,
    flatbuffers::Offset<flatbuffers::Vector<const Entity *>> entities = 0,
    flatbuffers::Offset<flatbuffers::Vector<int64_t>> names = 0) {
  SortedEntitiesBuilder builder_(_fbb);
  builder_.add_names(names);
  builder_.add_entities(entities);
  return builder_.Finish();
}

inline flatbuffers::Offset<SortedEntities> CreateSortedEntitiesDirect(
    flatbuffers::FlatBufferBuilder &_fbb,
    const std::vector<const Entity *> *entities = nullptr,
    const std::vector<int64_t> *names = nullptr) {
  return wordrep::wiki::io::CreateSortedEntities(
      _fbb,
      entities ? _fbb.CreateVector<const Entity *>(*entities) : 0,
      names ? _fbb.CreateVector<int64_t>(*names) : 0);
}

inline const wordrep::wiki::io::SortedEntities *GetSortedEntities(const void *buf) {
  return flatbuffers::GetRoot<wordrep::wiki::io::SortedEntities>(buf);
}

inline const char *SortedEntitiesIdentifier() {
  return "wfse";
}

inline bool SortedEntitiesBufferHasIdentifier(const void *buf) {
  return flatbuffers::BufferHasIdentifier(
      buf, SortedEntitiesIdentifier());
}

inline bool VerifySortedEntitiesBuffer(
    flatbuffers::Verifier &verifier) {
  return verifier.VerifyBuffer<wordrep::wiki::io::SortedEntities>(SortedEntitiesIdentifier());
}

inline const char *SortedEntitiesExtension() {
  return "wfse";
}

inline void FinishSortedEntitiesBuffer(
    flatbuffers::FlatBufferBuilder &fbb,
    flatbuffers::Offset<wordrep::wiki::io::SortedEntities> root) {
  fbb.Finish(root, SortedEntitiesIdentifier());
}

}  // namespace io
}  // namespace wiki
}  // namespace wordrep

#endif  // FLATBUFFERS_GENERATED_SORTEDENTITY_WORDREP_WIKI_IO_H_
