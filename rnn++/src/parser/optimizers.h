#pragma once
#include <memory>
#include <vector>
#include <string>

#include <lbfgs.h>

#include "parser/basic_type.h"
#include "utils/loop_gen.h" 
#include "parser/parser.h"
#include "parser/config.h"

namespace rnn{
//forward declarations
struct TokenizedSentences;

namespace simple_model{

//forward declarations
struct Param;
struct VocaInfo;

namespace optimizer{

struct GradientDescent{
    GradientDescent(rnn::type::float_t scale) : scale{scale} {}
    template<typename T, typename TV>
    void update(T &param, T const &grad, TV scale){
        grad_update(param,grad, scale); //defined in loop_gen.h
    }
    void update(Param &param, Param const &grad_sum);
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


class AdaGrad{    
public:
    using val_t = rnn::type::float_t;
    static const int word_dim=rnn::config::word_dim;
    AdaGrad(val_t scale);

    void update(Param &param, Param const &grad);
private:
    util::math::VecLoop_void<val_t,word_dim> vecloop_void{};
    util::math::MatLoop_void<val_t,word_dim,word_dim> matloop_void{};
    Param ada_factor{};
    val_t ada_scale;
};

class RMSprop{
public:
    using val_t = rnn::type::float_t;
    static const int word_dim=rnn::config::word_dim;
    RMSprop(val_t scale, WordBlock::idx_t voca_size);

    void update(Param &param, Param const &grad);
    void update(WordBlock &voca_vecs, SparseGrad const &grad);
private:
    WordBlock ada_factor_voca;
    Param ada_factor_param{};
    util::math::VecLoop_void<val_t,word_dim> vecloop_void{};
    util::math::MatLoop_void<val_t,word_dim,word_dim> matloop_void{};
    val_t ada_scale;
};

}//namespace rnn::simple_model::optimizer
}//namespace rnn::simple_model
}//namespace rnn
