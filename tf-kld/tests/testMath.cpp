#include "src/Type.h"

#include "utils/linear_algebra.h"
#include "utils/span.h"

#include "tests/testMath.h"

#include <vector>

using namespace util::math;

namespace tfkld{
namespace test{

void printdot() {

    std::vector<double> v1;
    std::vector<double> v2;
    
    for(int i = 0; i < 10; i++) {
        v1.push_back((double)1);
        v2.push_back((double)1);
    }

    gsl::span<double, 10> ve1{v1};
    gsl::span<double, 10> ve2{v2};

    std::cout << dot(ve1,ve2) << std::endl;

}

}
}
