#pragma once
#include "data_source/ygp_db.h"

#include "utils/json.h"

namespace data{
namespace ygp{

struct DBInfo{
    DBInfo(util::json_t const& config);



    YGPdb const db;
    DBIndexer const indexer;
    DBbyCountry const per_country;
    CountryCodeAnnotator const country_tagger;
};

}//data::ygp
}//data
