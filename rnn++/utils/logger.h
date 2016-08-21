#pragma once

#include <string>
#include <memory>

class Logger{
public:
    Logger(std::string name, std::string log_file);
    virtual ~Logger();
    void info(std::string log) const;
    void log_testscore(int idx, double score) const;
    std::string uid_str() const;
    // void debug(std::string log);
    // void warn(std::string log);
private:
    struct impl;
    std::unique_ptr<impl> pimpl;
    const int64_t uid;
};
