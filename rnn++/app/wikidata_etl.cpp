#include <sstream>
#include <iostream>

#include <fmt/printf.h>

#include "utils/parallel.h"
#include "utils/string.h"
#include "utils/json.h"

using util::get_str;
using util::find;
using util::has_key;

void count(std::istream&& is){
    tbb::task_group g;
    while (auto buffer=util::string::read_chunk(is, 2000000)) {
        auto& chars =  buffer.value();
        g.run([chars{std::move(chars)}](){
            std::stringstream ss;
            ss.str(chars.data());
            auto lines = util::string::readlines(std::move(ss));

            std::stringstream items;
            for(auto& line : lines){
                if(line.back()!=',') continue;
                line.back() = ' ';
                auto elm = util::json_t::parse(line);
                auto id = get_str(elm,"id");
                auto type = get_str(elm,"type");
                auto maybe_label = util::find<std::string>(elm["labels"]["en"], "value");

                std::stringstream head;
                fmt::print(head, "{}\t{}", id, type);
                if(has_key(elm["claims"], "P31")){
                    fmt::print(head, "\tP31");
                    for(auto& x : elm["claims"]["P31"]){
                        auto maybe_id = util::find<std::string>(x["mainsnak"]["datavalue"]["value"],"id");
                        if(!maybe_id) {
                            fmt::print(std::cerr, "{}\n", line);
                            continue;
                        }
                        auto id = maybe_id.value();
                        fmt::print(head, " {}", id);
                    }
                }
                if (has_key(elm["claims"], "P279")){
                    fmt::print(head, "\tP279");
                    for(auto& x : elm["claims"]["P279"]){
                        auto maybe_id = util::find<std::string>(x["mainsnak"]["datavalue"]["value"],"id");
                        if(!maybe_id) {
                            fmt::print(std::cerr, "{}\n", line);
                            continue;
                        }
                        auto id = maybe_id.value();
                        fmt::print(head, " {}", id);
                    }
                }

                if(maybe_label) {
                    fmt::print(items, "{}\t{}\n", head.str(), maybe_label.value());
                    for(auto& x : elm["aliases"]["en"])
                        fmt::print(items, "{}\t{}\n", id, get_str(x,"value"));
                }
            }
            fmt::print("{}", items.str());
        });
    }
    g.wait();
}

int main(){
    count(std::move(std::cin));
    return 0;
}
