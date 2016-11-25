#include "data_source/corenlp_helper.h"

#include <sstream>
#include <fstream>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>

#include <sys/wait.h>

#include "utils/random.h"

namespace {

//TODO: It does not work. Waiting mechanism should be fixed.
static int exec_prog(const char **argv) {
    pid_t   this_pid;
    int     status, timeout;
    char *envp[] = { "PYTHONPATH=/nix/store/8xvsall0dpnwlgkbiklgzivi5h8myn59-python2.7-requests-2.9.1/lib/python2.7/site-packages", 0 };

    if (0 == (this_pid = fork())) {
        //TODO: Temporal directory in /nix/store is hard coded...
        if (-1 == execve("/nix/store/mbf2m45krqpdx45haa6rn9ipwwivlp16-python-2.7.11/bin/python", (char **)argv , envp)) {
            perror("child process execve failed [%m]");
            return -1;
        }
    }

    timeout = 10;
    while (0 == waitpid(this_pid , &status , WNOHANG)) {
            if ( --timeout < 0 ) {
                    perror("timeout");
                    return -1;
            }
            sleep(1);
    }
    if (1 != WIFEXITED(status) || 0 != WEXITSTATUS(status)) return -1;

    return 0;
}

}//nameless namespace
namespace data{

util::json_t CoreNLPwebclient::from_query_file(std::string content_file_path) const {
    std::string command = "python "+script_path + "  "+content_file_path;
    int ret=system(command.c_str());
//    const char    *my_argv[64] = {"python" , script_path.c_str() , content_file_path.c_str() , NULL};
//    int ret = exec_prog(my_argv);
    return util::load_json(content_file_path+".corenlp");
}
util::json_t CoreNLPwebclient::from_query_content(std::string query_content) const {
    std::string content_file_path = "tmp."+util::get_uuid_str();
    std::ofstream temp_file;
    temp_file.open (content_file_path);
    temp_file << query_content;
    temp_file.close();
    auto query_json = from_query_file(content_file_path);
    auto commend_remove_temp_file = "rm -f "+content_file_path +"*";
    system(commend_remove_temp_file.c_str());
    return query_json;
}

}//namespace data
