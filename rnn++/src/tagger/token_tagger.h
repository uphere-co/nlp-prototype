#pragma once

#include "wordrep/sentence.h"
#include "wordrep/dep_parsed.h"
#include "wiki/wikidata.h"

namespace tagger {

wordrep::AnnotatedSentence test_program(wikidata::GreedyAnnotator& ann, util::json_t const& p );
    

}//namespace tagger
