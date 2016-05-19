#include <algorithm>
#include <iostream>
#include <iterator>
#include <fstream>
#include <string>
#include <vector>

using namespace std;

void Tokenize(const string& str, vector<string>& tokens, const string& delimiters = " "){
  string::size_type lastPos = str.find_first_not_of(delimiters, 0);
  string::size_type pos = str.find_first_of(delimiters, lastPos);

  while (string::npos != pos || string::npos != lastPos){
    tokens.push_back(str.substr(lastPos, pos - lastPos));
    lastPos = str.find_first_not_of(delimiters, pos);
    pos = str.find_first_of(delimiters, lastPos);
  }
}

int main()
{
  char inputString[1000];

  string buf;

  vector<string> tokens;
  
  ifstream inFile("input.txt");
  ofstream outFile("outfile.txt");
  
  while(!inFile.eof()){
    inFile.getline(inputString,1000);
    Tokenize(inputString, tokens);
  }

  for(vector<string>::size_type i = 0; i < tokens.size(); ++i)
   outFile << tokens[i] << endl;

  
  inFile.close();
  outFile.close();


  
  return 0;
}
