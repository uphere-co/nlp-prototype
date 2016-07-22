#ifndef UTILS_H
#define UTILS_H

#include <algorithm>
#include <string>
#include <vector>
#include <iterator>

// Begin of Tokenizer

void makeLower(std::string& str);
void remove_punctuations(std::string& str);
void remove_stopwords(std::vector<std::string>& tokens);
void Tokenize(const std::string& str, std::vector<std::string>& tokens);
void split(const std::string& str, std::vector<std::string>& tokens);
// End of Tokenizer

#endif /* UTILS_H */
