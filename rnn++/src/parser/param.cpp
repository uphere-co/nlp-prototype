#include "parser/param.h"

#include <gsl/gsl.h>
#include "utils/span.h"

#include "utils/hdf5.h" 
#include "utils/loop_gen.h"

namespace rnn{
namespace simple_model{

Param::raw_type Param::serialize() const{
    auto wLT       = util::math::transpose(w_left.span);
    auto wRT       = util::math::transpose(w_right.span);
    raw_type raw_data(dim*(dim*2+2));
    std::copy(wLT.span.cbegin(),wLT.span.cend(), raw_data.data());
    std::copy(wRT.span.cbegin(),wRT.span.cend(), raw_data.data()+dim*dim);
    std::copy(bias.span.cbegin(),bias.span.cend(), raw_data.data()+2*dim*dim);
    std::copy(u_score.span.cbegin(),u_score.span.cend(), raw_data.data()+(1+2*dim)*dim);
    // bias._val[0]=1;
    //TODO: fix bug in Param::serialize. span assignment should be disabled in const methods.
    //  bias.span[0]=1;
    return raw_data;
}
void write_to_disk(Param const &param, std::string param_name){
    using namespace util::io;    
    auto param_raw = param.serialize();
    H5file h5store{H5name{"rnn_params.h5"}, hdf5::FileMode::rw_exist};
    h5store.writeRawData(H5name{param_name}, param_raw);
}

Param deserializeParam(std::vector<rnn::type::float_t> &param_raw){
    constexpr auto dim = rnn::config::word_dim;
    auto span2d   = gsl::as_span(param_raw.data(), util::dim<dim*2+2>(),util::dim<dim>());
    // auto w_span = gsl::as_span(span2d.data(), util::dim<dim*2>(),util::dim<dim>());
    auto wLT_span = gsl::as_span(span2d.data(),      util::dim<dim>(),util::dim<dim>());
    auto wRT_span = gsl::as_span(span2d[dim].data(), util::dim<dim>(),util::dim<dim>());
    auto wL       = util::math::transpose(wLT_span);
    auto wR       = util::math::transpose(wRT_span);
    auto bias     = util::math::Vector<rnn::type::float_t,dim>{span2d[2*dim]};
    auto u_score  = util::math::Vector<rnn::type::float_t,dim>{span2d[2*dim+1]};
    return Param{std::move(wL), std::move(wR), std::move(bias), std::move(u_score)};
}
Param randomParam(Param::value_type scale){
    std::random_device rd;
    //std::mt19937 e{rd()};
    std::mt19937 e{}; //fixed seed for testing.
    std::uniform_real_distribution<Param::value_type>  uniform_dist{-scale, scale};
    auto dim = Param::dim;
    std::vector<Param::value_type> param_raw(dim*(dim*2+2));
    for(auto &x : param_raw)
        x=uniform_dist(e);
    return deserializeParam(param_raw);
}

Param load_param(std::string const &h5_name,  
                 std::string const &param_name, util::DataType param_type) {
    using namespace rnn::config;
    using namespace util::io;
    H5file param_storage{H5name{h5_name}, hdf5::FileMode::read_exist};
    std::vector<rnn::type::float_t> param_raw;
    if(param_type == util::DataType::sp){
        auto param_raw0 = param_storage.getRawData<float>(H5name{param_name});
        for(auto x: param_raw0) param_raw.push_back(x);
    } else if(param_type == util::DataType::dp){
        auto param_raw0 = param_storage.getRawData<double>(H5name{param_name});
        for(auto x: param_raw0) param_raw.push_back(x);
    }
    return rnn::simple_model::deserializeParam(param_raw);
}

Param& operator +=(Param& out, const Param& x){
    out.w_left.span  += x.w_left.span;
    out.w_right.span += x.w_right.span;
    out.bias.span    += x.bias.span;
    out.u_score.span += x.u_score.span;
    return out;
};
Param& operator -=(Param& out, const Param& x){
    out.w_left.span  -= x.w_left.span;
    out.w_right.span -= x.w_right.span;
    out.bias.span    -= x.bias.span;
    out.u_score.span -= x.u_score.span;
    return out;
};
Param& operator *=(Param& out, Param::value_type scale){
    out.w_left.span  *= scale;
    out.w_right.span *= scale;
    out.bias.span    *= scale;
    out.u_score.span *= scale;
    return out;
};

Param operator +(const Param& x, const Param& y){
    Param out{x};
    out += y;
    return out;
};
Param operator -(const Param& x, const Param& y){
    Param out{x};
    out -= y;
    return out;
};
// Param operator *(Param::value_type x, const Param& y){
//     Param out{y};
//     out *= x;
//     return out;
// };

}//namespace rnn::simple_model
}//namespace rnn


