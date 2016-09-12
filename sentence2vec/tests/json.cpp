#include <json/json.hpp>
#include <fstream>

using json = nlohmann::json;

int main()
{
    std::ifstream f("test.json", std::ifstream::in);
    json j;

    if(f.is_open()) {
        f >> j;
    }

    // pretty print with indent of 4 spaces
    std::cout << std::setw(4) << j << '\n';
    return 0;
}
