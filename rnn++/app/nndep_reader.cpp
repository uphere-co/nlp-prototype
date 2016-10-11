#include <vector>
#include <algorithm>

#include "fmt/printf.h"

#include "parser/voca.h"
#include "parser/parser.h"
#include "similarity/similarity.h"

#include "utils/json.h"
#include "utils/hdf5.h"
#include "utils/profiling.h"
#include "utils/span.h"
#include "utils/string.h"

using namespace util::io;

int main(int /*argc*/, char** argv){
//    convert_h5py_to_native();
    auto config = util::load_json(argv[1]);
    auto query_json = util::load_json(argv[2]);
    util::Timer timer{};
    DepParseSearch engine{config};
    timer.here_then_reset("Data loaded.");
    auto answer = engine.process_queries(query_json);
    timer.here_then_reset("Queries are answered.");
    fmt::print("{}\n", answer.dump(4));
    return 0;
}
