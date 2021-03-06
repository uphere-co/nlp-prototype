// automatically generated by the FlatBuffers compiler, do not modify


#ifndef FLATBUFFERS_GENERATED_ENTITY_UTIL_IO_H_
#define FLATBUFFERS_GENERATED_ENTITY_UTIL_IO_H_

#include "flatbuffers/flatbuffers.h"

namespace util {
namespace io {

struct Entity;

struct Entities;

struct Entity FLATBUFFERS_FINAL_CLASS : private flatbuffers::Table {
  enum {
    VT_UID = 4,
    VT_LEN_NAME = 6
  };
  int64_t uid() const {
    return GetField<int64_t>(VT_UID, 0);
  }
  int16_t len_name() const {
    return GetField<int16_t>(VT_LEN_NAME, 0);
  }
  bool Verify(flatbuffers::Verifier &verifier) const {
    return VerifyTableStart(verifier) &&
           VerifyField<int64_t>(verifier, VT_UID) &&
           VerifyField<int16_t>(verifier, VT_LEN_NAME) &&
           verifier.EndTable();
  }
};

struct EntityBuilder {
  flatbuffers::FlatBufferBuilder &fbb_;
  flatbuffers::uoffset_t start_;
  void add_uid(int64_t uid) {
    fbb_.AddElement<int64_t>(Entity::VT_UID, uid, 0);
  }
  void add_len_name(int16_t len_name) {
    fbb_.AddElement<int16_t>(Entity::VT_LEN_NAME, len_name, 0);
  }
  EntityBuilder(flatbuffers::FlatBufferBuilder &_fbb)
        : fbb_(_fbb) {
    start_ = fbb_.StartTable();
  }
  EntityBuilder &operator=(const EntityBuilder &);
  flatbuffers::Offset<Entity> Finish() {
    const auto end = fbb_.EndTable(start_, 2);
    auto o = flatbuffers::Offset<Entity>(end);
    return o;
  }
};

inline flatbuffers::Offset<Entity> CreateEntity(
    flatbuffers::FlatBufferBuilder &_fbb,
    int64_t uid = 0,
    int16_t len_name = 0) {
  EntityBuilder builder_(_fbb);
  builder_.add_uid(uid);
  builder_.add_len_name(len_name);
  return builder_.Finish();
}

struct Entities FLATBUFFERS_FINAL_CLASS : private flatbuffers::Table {
  enum {
    VT_ENTITIES = 4,
    VT_NAMES = 6
  };
  const flatbuffers::Vector<flatbuffers::Offset<Entity>> *entities() const {
    return GetPointer<const flatbuffers::Vector<flatbuffers::Offset<Entity>> *>(VT_ENTITIES);
  }
  const flatbuffers::Vector<int64_t> *names() const {
    return GetPointer<const flatbuffers::Vector<int64_t> *>(VT_NAMES);
  }
  bool Verify(flatbuffers::Verifier &verifier) const {
    return VerifyTableStart(verifier) &&
           VerifyField<flatbuffers::uoffset_t>(verifier, VT_ENTITIES) &&
           verifier.Verify(entities()) &&
           verifier.VerifyVectorOfTables(entities()) &&
           VerifyField<flatbuffers::uoffset_t>(verifier, VT_NAMES) &&
           verifier.Verify(names()) &&
           verifier.EndTable();
  }
};

struct EntitiesBuilder {
  flatbuffers::FlatBufferBuilder &fbb_;
  flatbuffers::uoffset_t start_;
  void add_entities(flatbuffers::Offset<flatbuffers::Vector<flatbuffers::Offset<Entity>>> entities) {
    fbb_.AddOffset(Entities::VT_ENTITIES, entities);
  }
  void add_names(flatbuffers::Offset<flatbuffers::Vector<int64_t>> names) {
    fbb_.AddOffset(Entities::VT_NAMES, names);
  }
  EntitiesBuilder(flatbuffers::FlatBufferBuilder &_fbb)
        : fbb_(_fbb) {
    start_ = fbb_.StartTable();
  }
  EntitiesBuilder &operator=(const EntitiesBuilder &);
  flatbuffers::Offset<Entities> Finish() {
    const auto end = fbb_.EndTable(start_, 2);
    auto o = flatbuffers::Offset<Entities>(end);
    return o;
  }
};

inline flatbuffers::Offset<Entities> CreateEntities(
    flatbuffers::FlatBufferBuilder &_fbb,
    flatbuffers::Offset<flatbuffers::Vector<flatbuffers::Offset<Entity>>> entities = 0,
    flatbuffers::Offset<flatbuffers::Vector<int64_t>> names = 0) {
  EntitiesBuilder builder_(_fbb);
  builder_.add_names(names);
  builder_.add_entities(entities);
  return builder_.Finish();
}

inline flatbuffers::Offset<Entities> CreateEntitiesDirect(
    flatbuffers::FlatBufferBuilder &_fbb,
    const std::vector<flatbuffers::Offset<Entity>> *entities = nullptr,
    const std::vector<int64_t> *names = nullptr) {
  return util::io::CreateEntities(
      _fbb,
      entities ? _fbb.CreateVector<flatbuffers::Offset<Entity>>(*entities) : 0,
      names ? _fbb.CreateVector<int64_t>(*names) : 0);
}

inline const util::io::Entities *GetEntities(const void *buf) {
  return flatbuffers::GetRoot<util::io::Entities>(buf);
}

inline const char *EntitiesIdentifier() {
  return "ufwe";
}

inline bool EntitiesBufferHasIdentifier(const void *buf) {
  return flatbuffers::BufferHasIdentifier(
      buf, EntitiesIdentifier());
}

inline bool VerifyEntitiesBuffer(
    flatbuffers::Verifier &verifier) {
  return verifier.VerifyBuffer<util::io::Entities>(EntitiesIdentifier());
}

inline const char *EntitiesExtension() {
  return "ufwe";
}

inline void FinishEntitiesBuffer(
    flatbuffers::FlatBufferBuilder &fbb,
    flatbuffers::Offset<util::io::Entities> root) {
  fbb.Finish(root, EntitiesIdentifier());
}

}  // namespace io
}  // namespace util

#endif  // FLATBUFFERS_GENERATED_ENTITY_UTIL_IO_H_
