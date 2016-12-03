#include "wordrep/sentence.h"

#include "wordrep/dep_parsed.h"

namespace wordrep {

CharOffset Sentence::beg_offset() const {return tokens->word_beg(front());}
CharOffset Sentence::end_offset() const {return tokens->word_end(back());}
SentUID::val_t Sentence::chrlen() const{ return util::diff(end_offset(), beg_offset());}

}//namespace wordrep
