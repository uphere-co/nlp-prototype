#include <random>
#include <sstream>

#include <spdlog/spdlog.h>

#include "utils/logger.h"

namespace{
int64_t uuid_gen(){    
    std::random_device rd{};
    std::mt19937 e{rd()};
    std::uniform_int_distribution<int64_t>  uniform_dist{1, 0xffffffff};
    return uniform_dist(e);
}
}//nameless namespace

struct Logger::impl{
    impl(std::string name, std::string log_file)
    : logger{spdlog::basic_logger_mt(name, log_file)} {}
    std::shared_ptr<spdlog::logger> logger; 
};

Logger::Logger(std::string name, std::string log_file)
: pimpl{std::make_unique<impl>(name,log_file)}, uid{uuid_gen()} {
    // %n	Logger's name	
    // %l	The log level of the message
    // %t	Thread id
    // %Y	Year in 4 digits
    // %m	Month 1-12
    // %d	Day of month 1-31
    // %z	ISO 8601 offset from UTC in timezone ([+/-]HH:MM)
    pimpl->logger->set_pattern("%Y-%m-%d %H:%M:%S:%f%z %n %l %v");
}
Logger::~Logger(){}

std::string Logger::uid_str() const{
    std::stringstream ss;
    ss << std::hex << uid;
    return ss.str();
}

void Logger::info(std::string log) const {
    pimpl->logger->info("{0:08x} {1}", uid, log);
    pimpl->logger->flush();
}

void Logger::log_testscore(int idx, double score) const {
    pimpl->logger->info("{0:08x} {1}-th mini-batch, {2}", uid, idx, score);
    pimpl->logger->flush();
}
