#pragma once

#include <lbfgs.h>

#include "parser/parser.h"

#include "parser/basic_type.h"

namespace rnn{
namespace simple_model{
namespace optimizer{

struct GradientDescent{
    GradientDescent(rnn::type::float_t scale) : scale{scale} {}
    template<typename T, typename TV>
    void update(T &param, T const &grad, TV scale){
        grad_update(param,grad, scale);
    }
    void update(Param &param, Param &grad_sum);
    rnn::type::float_t scale;
};

class LBFGSoptimizer{
    using lfloat_t = lbfgsfloatval_t;
    using c_iter   = TokenizedSentences::c_iter;
public:
    LBFGSoptimizer(int n_dim, Param &param, VocaInfo const &rnn, 
                   TokenizedSentences const &testset, c_iter beg, c_iter end);
    virtual ~LBFGSoptimizer() {lbfgs_free(m_x);}

    int update();
protected:
    lfloat_t evaluate(const lfloat_t *x, lfloat_t *g, const int /*n*/, const lfloat_t /*step*/);

    
    int n_dim;
    lfloat_t *m_x;
    lbfgs_parameter_t lbfgs_param;

    Param &param;
    VocaInfo const &rnn;
    TokenizedSentences const &testset;
    c_iter beg;
    c_iter end;
};


}//namespace rnn::simple_model::optimizer
}//namespace rnn::simple_model
}//namespace rnn
