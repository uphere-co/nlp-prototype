#pragma once

#include "utils/base_types.h"

namespace wordrep {

struct DummyWordUID{};
using WordUID = util::IntegerLike<DummyWordUID,-1>; //UID -1 for unknown words.
struct DummyPOSUID{};
using POSUID = util::IntegerLike<DummyPOSUID,-1>; //UID -1 for unknown words.
struct DummyArcLabelUID{};
using ArcLabelUID = util::IntegerLike<DummyArcLabelUID,-1>; //UID -1 for unknown words.

struct WordPosIndexDummy {};
using WordPosition = util::IntegerLike<WordPosIndexDummy>;
struct SentPositionDummy {};
using SentIndex = util::IntegerLike<SentPositionDummy>;
struct SentUIDDummy {};
using SentUID = util::IntegerLike<SentUIDDummy>;

struct CharOffsetDummy;
using CharOffset = util::IntegerLike<CharOffsetDummy>;

struct DPTokenIndexDummy;
using DPTokenIndex = util::IntegerLike<DPTokenIndexDummy>;
struct ChunkIndexDummy;
using ChunkIndex = util::IntegerLike<ChunkIndexDummy>;

}//namespace wordrep