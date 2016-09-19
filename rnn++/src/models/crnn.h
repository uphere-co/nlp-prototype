#pragma once

#include <vector>
#include <array>

#include "utils/linear_algebra.h"
#include "utils/span.h"

#include "rnn/common.h"

namespace rnn{
namespace model {
namespace crnn {

template<typename FLOAT, int WORD_DIM, int LEN_CTX>
struct Context {
    using val_t = FLOAT;
    using node_t = rnn::detail::Node<Context>;
    using raw_t = std::vector<val_t>;
    using vec_t = util::span_1d<val_t, WORD_DIM>;
    static constexpr auto len_context = LEN_CTX;
    static constexpr auto word_dim = WORD_DIM;

    Context(raw_t &&vec, util::cstring_span<> name)
            : raw{std::move(vec)}, vspan{raw}, name{name},
              vec{vspan.subspan(0, WORD_DIM)},
              vec_wsum{vspan.subspan(WORD_DIM, WORD_DIM)},
              vec_update{vspan.subspan(2 * WORD_DIM, WORD_DIM)} {
        for (auto &x:left_ctxs) x = nullptr;
        for (auto &x:right_ctxs) x = nullptr;
        assert(raw.size() == 3 * WORD_DIM);
    }

    Context(raw_t const &raw, util::cstring_span<> name)
            : Context(raw_t{raw}, name) {}

    Context()
            : Context(std::move(raw_t(3 * WORD_DIM)), {}) {}

    Context(util::cstring_span<> name, vec_t word_vec)
            : Context(std::move(raw_t(3 * WORD_DIM)), name) {
        std::copy(word_vec.cbegin(), word_vec.cend(), raw.begin());
    }

    Context(Context const &orig)
            : Context(orig.raw, orig.name) {
        std::copy(orig.left_ctxs.cbegin(), orig.left_ctxs.cend(), left_ctxs.begin());
        std::copy(orig.right_ctxs.cbegin(), orig.right_ctxs.cend(), right_ctxs.begin());
        score = orig.score;
    }

    Context &operator=(Context const &orig) {
        assert(raw.size() == 3 * WORD_DIM);
        std::copy(orig.raw.cbegin(), orig.raw.cend(), raw.begin());
        std::copy(orig.left_ctxs.cbegin(), orig.left_ctxs.cend(), left_ctxs.begin());
        std::copy(orig.right_ctxs.cbegin(), orig.right_ctxs.cend(), right_ctxs.begin());
        name = orig.name;
        score = orig.score;
        return *this;
    }

    Context(Context &&orig)
            : Context(std::move(orig.raw), orig.name) {
        std::copy(orig.left_ctxs.cbegin(), orig.left_ctxs.cend(), left_ctxs.begin());
        std::copy(orig.right_ctxs.cbegin(), orig.right_ctxs.cend(), right_ctxs.begin());
        score = orig.score;
    }

    void set_context(util::span_dyn <node_t> lefts,
                     util::span_dyn <node_t> rights) {
        //TODO:Simplify this
        auto m = lefts.length();
        for (decltype(m) i = 0; i < m; ++i) left_ctxs[i] = &lefts[i];
        auto n = rights.length();
        for (decltype(n) i = 0; i < n; ++i) right_ctxs[i] = &rights[i];
    }

    raw_t raw;
    util::span_1d<val_t, 3 * WORD_DIM> vspan;
    std::array<node_t const *, LEN_CTX> left_ctxs;
    std::array<node_t const *, LEN_CTX> right_ctxs;
    util::cstring_span<> name;
    vec_t vec;
    vec_t vec_wsum;
    vec_t vec_update;
    val_t score{0.0};
};

template<typename FLOAT, int WORD_DIM, int LEN_CTX>
struct Param {
    static constexpr auto dim = WORD_DIM;
    static constexpr auto len_context = LEN_CTX;
    static constexpr auto d_ext = util::dim<dim>();
    static constexpr auto lc_ext = util::dim<len_context>();

    using val_t = FLOAT;
    using raw_t = std::vector<val_t>;
    using mats_t= util::span_3d<val_t, len_context, dim, dim>;
    using mat_t = util::span_2d<val_t, dim, dim>;
    using vec_t = util::span_1d<val_t, dim>;
    using mesg_t = util::math::Vector<val_t, dim>;

    static Param random(val_t scale) {
//        std::random_device rd;
//        std::mt19937 e{rd()};
        std::mt19937 e{}; //fixed seed for testing.
        std::uniform_real_distribution <val_t> uniform_dist{-scale, scale};
        raw_t param_raw(dim * dim * (2 + 2 * len_context) + dim * 2);
        for (auto &x : param_raw) x = uniform_dist(e);
        return Param{std::move(param_raw)};
    }

    Param(raw_t &&raw)
            : _val(std::move(raw)), span{_val},
              w_context_left{util::as_span(span.subspan(0, len_context * dim * dim), lc_ext, d_ext, d_ext)},
              w_left{util::as_span(span.subspan(len_context * dim * dim, dim * dim), d_ext, d_ext)},
              w_right{util::as_span(span.subspan((1 + len_context) * dim * dim, dim * dim), d_ext, d_ext)},
              w_context_right{
                      util::as_span(span.subspan((2 + len_context) * dim * dim, len_context * dim * dim), lc_ext, d_ext,
                                    d_ext)},
              bias{util::as_span(span.subspan((2 + 2 * len_context) * dim * dim, dim), d_ext)},
              u_score{util::as_span(span.subspan((2 + 2 * len_context) * dim * dim + dim, dim), d_ext)} {}

    Param()
            : Param(std::move(raw_t(dim * dim * (2 + 2 * len_context) + dim * 2, 0))) {}

    Param(Param const &orig)
            : Param(std::move(raw_t{orig._val})) {}

    raw_t serialize() const { return _val; };


    Param &operator+=(Param const &x) {
        span += x.span;
        return *this;
    }

    Param &operator-=(Param const &x) {
        span -= x.span;
        return *this;
    }

    Param &operator*=(Param::val_t x) {
        span *= x;
        return *this;
    }

    raw_t _val;
    util::span_1d<val_t, dim * dim * (2 + 2 * len_context) + dim * 2> span;
    mats_t w_context_left;
    mat_t w_left;
    mat_t w_right;
    mats_t w_context_right;
    vec_t bias;
    vec_t u_score;
};

template<typename FLOAT, int WORD_DIM, int LEN_CTX>
Param<FLOAT,WORD_DIM,LEN_CTX> operator+(Param<FLOAT,WORD_DIM,LEN_CTX> const &x,
                                        Param<FLOAT,WORD_DIM,LEN_CTX> const &y) {
    Param<FLOAT,WORD_DIM,LEN_CTX> out{x};
    out.span += y.span;
    return out;
}

template<typename FLOAT, int WORD_DIM, int LEN_CTX>
Param<FLOAT,WORD_DIM,LEN_CTX> operator-(Param<FLOAT,WORD_DIM,LEN_CTX> const &x,
                                        Param<FLOAT,WORD_DIM,LEN_CTX> const &y) {
    Param<FLOAT,WORD_DIM,LEN_CTX> out{x};
    out.span -= y.span;
    return out;
}

template<typename FLOAT, int WORD_DIM, int LEN_CTX>
Param<FLOAT,WORD_DIM,LEN_CTX> operator*(Param<FLOAT,WORD_DIM,LEN_CTX> const &x,
                                        typename Param<FLOAT,WORD_DIM,LEN_CTX>::val_t y) {
    Param<FLOAT,WORD_DIM,LEN_CTX> out{x};
    out.span *= y;
    return out;
}
}//namespace rnn::model::crnn
}//namespace rnn::model
}//namespace rnn

