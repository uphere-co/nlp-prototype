#pragma once
#include <memory>
#include <vector>
#include <string>

#include <lbfgs.h>

#include "parser/basic_type.h"
#include "utils/loop_gen.h" 

namespace rnn{
namespace simple_model{

//forward declarations
struct Param;
struct VocaInfo;
struct TokenizedSentences;

namespace optimizer{

struct GradientDescent{
    GradientDescent(rnn::type::float_t scale) : scale{scale} {}
    template<typename T, typename TV>
    void update(T &param, T const &grad, TV scale){
        grad_update(param,grad, scale); //defined in loop_gen.h
    }
    void update(Param &param, Param &grad_sum);
    rnn::type::float_t scale;
};

class LBFGSoptimizer{
    using lfloat_t = lbfgsfloatval_t;
    using c_iter   =  std::vector<std::string>::const_iterator;
public:
    LBFGSoptimizer(int n_dim, Param &param, VocaInfo const &rnn, 
                   TokenizedSentences const &testset, c_iter beg, c_iter end);
    virtual ~LBFGSoptimizer();
    int update();
protected:
    lfloat_t evaluate(const lfloat_t *x, lfloat_t *g, const int /*n*/, const lfloat_t /*step*/);

    int n_dim;
    lfloat_t *m_x;
    lbfgs_parameter_t lbfgs_param;

    struct impl;
    std::unique_ptr<impl> pimpl;
};


}//namespace rnn::simple_model::optimizer
}//namespace rnn::simple_model
}//namespace rnn
