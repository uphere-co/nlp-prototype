#pragma once

#include "wordrep/sentence.h"
#include "data_source/db_query.h"

namespace engine{

struct SentenceQuery{
    wordrep::Sentence sent;
    data::QuerySentInfo info;
};

}//namespace engine
