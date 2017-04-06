#include <cassert>
#include "data_source/ygp_etl.h"

int main(int argc, char** argv){
    assert(argc>2);
    auto columns_to_export = argv[1];
    auto country_columns_file = argv[2];
    auto dump_path=argv[3];
    data::ygp::generate_country_columns(dump_path, country_columns_file);
    data::ygp::dump_psql(columns_to_export, dump_path);
    return 0;
}
