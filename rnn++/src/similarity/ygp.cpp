#include "similarity/ygp.h"

#include "utils/versioned_name.h"
#include "utils/hdf5.h"

using util::io::h5read;

namespace data{
namespace ygp{

DBInfo::DBInfo(util::json_t const& config)
        : db{config["column_uids_dump"].get<std::string>()},
          indexer{h5read(util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname),
                  config["dep_parsed_prefix"].get<std::string>()},
          per_country{h5read(util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname),
                      util::get_latest_version(util::get_str(config, "country_uids_dump")).fullname},
          country_tagger{util::get_latest_version(util::get_str(config, "country_uids_dump")).fullname}
{}

}//data::ygp
}//data
