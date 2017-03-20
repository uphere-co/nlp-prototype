#include <cassert>

#include "data_source/ygp_etl.h"

int main(int argc, char** argv){
    assert(argc>1);
    auto columns_to_export = argv[1];
    std::string dump_path="";
    if(argc>2) dump_path=argv[2];
    //auto config = util::load_json(argv[1]);
    //auto columns_to_export = util::get_str(config,"column_uids_dump")
    data::ygp::dump_psql(columns_to_export, dump_path);

    return 0;
}
