#include <sstream>

#include <fmt/printf.h>

#include "utils/parallel.h"
#include "utils/string.h"
#include "utils/json.h"

using util::get_str;
using util::find;

void count(std::istream&& is){
    tbb::task_group g;
    while (auto buffer=util::string::read_chunk(is, 2000000)) {
        auto& chars =  buffer.value();
        g.run([chars{std::move(chars)}](){
            std::stringstream ss;
            ss.str(chars.data());
            auto lines = util::string::readlines(std::move(ss));
            for(auto& line : lines){
                if(line.back()!=',') continue;
                line.back() = ' ';
                auto elm = util::json_t::parse(line);
                std::stringstream ss;
                fmt::print(ss, "{}\t{}", get_str(elm,"id"), get_str(elm,"type"));
                auto maybe_label = util::find<std::string>(elm["labels"]["en"], "value");
                if(maybe_label) {
                    fmt::print(ss, "\t{}", maybe_label.value());
                    for(auto& x : elm["aliases"]["en"]) fmt::print(ss, "\t{}", get_str(x,"value"));
                }
                fmt::print("{}\n", ss.str());
            }
        });
    }
    g.wait();
}

int main(){
    count(std::move(std::cin));
    return 0;
}
