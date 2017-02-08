#include <sstream>

#include <fmt/printf.h>

#include "utils/parallel.h"
#include "utils/string.h"
#include "utils/json.h"

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
                fmt::print("{}\n", util::get_str(elm,"id"));
            }
        });
    }
    g.wait();
}

int main(){
    count(std::move(std::cin));
    return 0;
}
