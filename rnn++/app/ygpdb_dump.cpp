#include "data_source/ygp_etl.h"

int main(int /*argc*/, char** argv){
    auto columns_to_export = argv[1];
    //auto config = util::load_json(argv[1]);
    //auto columns_to_export = util::get_str(config,"column_uids_dump")
    data::ygp::dump_psql(columns_to_export);

    return 0;
}
