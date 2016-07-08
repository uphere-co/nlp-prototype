#include "utils.h"

// Begin of Tokenizer

void makeLower(std::string& str) {
  std::transform(str.begin(), str.end(), str.begin(), ::tolower);
}


void remove_punctuations(std::string& str) {
  std::vector<char> punctuations {'.',',','?',':',';','!'}; // 6 punctuations
    for(std::vector<char>::const_iterator it = punctuations.begin(); it != punctuations.end(); ++it) {
      str.erase(std::remove(str.begin(),str.end(), (*it)), str.end());
   }
}


void remove_stopwords(std::vector<std::string>& tokens) {
  std::vector<std::string> stopwords {"a","able","about","across","after","all","almost","also","am","among","an","and","any","are","as","at","be","because","been","but","by","can","cannot","could","dear","did","do","does","either","else","ever","every","for","from","get","got","had","has","have","he","her","hers","him","his","how","however","i","if","in","into","is","it","its","just","least","let","like","likely","may","me","might","most","must","my","neither","no","nor","not","of","off","often","on","only","or","other","our","own","rather","said","say","says","she","should","since","so","some","than","that","the","their","them","then","there","these","they","this","tis","to","too","twas","us","wants","was","we","were","what","when","where","which","while","who","whom","why","will","with","would","yet","you","your"}; // 119 stopwords.

  for(std::vector<std::string>::const_iterator it = stopwords.begin(); it != stopwords.end(); ++it) {
    if(std::find(tokens.begin(), tokens.end(), (*it)) != tokens.end())
      tokens.erase(std::remove(tokens.begin(), tokens.end(), (*it)), tokens.end());  
  }
}


void Tokenize(const std::string& str, std::vector<std::string>& tokens) {
  const std::string delimiters = " ";
  
  std::string::size_type lastPos = str.find_first_not_of(delimiters, 0);
  std::string::size_type pos= str.find_first_of(delimiters, lastPos);

  while (std::string::npos != pos || std::string::npos != lastPos) {
    std::string a = str.substr(lastPos, pos-lastPos);
    //makeLower(a); // Converting into lowercase characters.
    //remove_punctuations(a);
    if(std::find(tokens.begin(), tokens.end(), a) == tokens.end()) // find() returns tokens.end if tarket is not found. 
      tokens.push_back(a);
    lastPos = str.find_first_not_of(delimiters, pos);
    pos = str.find_first_of(delimiters, lastPos);
  }
  remove_stopwords(tokens);
}

void split(const std::string& str, std::vector<std::string>& tokens) {
  const std::string delimiters = " ";
  
  std::string::size_type lastPos = str.find_first_not_of(delimiters, 0);
  std::string::size_type pos= str.find_first_of(delimiters, lastPos);

  while (std::string::npos != pos || std::string::npos != lastPos) {
    std::string a = str.substr(lastPos, pos-lastPos);
    tokens.push_back(a);
    lastPos = str.find_first_not_of(delimiters, pos);
    pos = str.find_first_of(delimiters, lastPos);
  }
}

// End of Tokenizer
