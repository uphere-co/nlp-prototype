#include <algorithm>
#include <cmath>
#include <iostream>
#include <iterator>
#include <fstream>
#include <string>
#include <vector>

using namespace std;

const int vnum = 4;
const int nnum = 2;

#define MAX_STRING 1000

void makeLower(string& str){
  std::transform(str.begin(), str.end(), str.begin(), ::tolower);
}

void remove_punctuations(string& str){
  char punctuations[5] = {'.',',','?',':',';'};
  for(int i=0;i<5;i++)
    str.erase(std::remove(str.begin(),str.end(), punctuations[i]), str.end());
}

void remove_stopwords(vector<string>& tokens){
  vector<string> stopwords {"a","able","about","across","after","all","almost","also","am","among","an","and","any","are","as","at","be","because","been","but","by","can","cannot","could","dear","did","do","does","either","else","ever","every","for","from","get","got","had","has","have","he","her","hers","him","his","how","however","i","if","in","into","is","it","its","just","least","let","like","likely","may","me","might","most","must","my","neither","no","nor","not","of","off","often","on","only","or","other","our","own","rather","said","say","says","she","should","since","so","some","than","that","the","their","them","then","there","these","they","this","tis","to","too","twas","us","wants","was","we","were","what","when","where","which","while","who","whom","why","will","with","would","yet","you","your"}; // 119 stopwords.

  for(vector<string>::const_iterator iter = stopwords.begin(); iter != stopwords.end(); ++iter) {
    if(std::find(tokens.begin(), tokens.end(), (*iter)) != tokens.end())
      tokens.erase(std::remove(tokens.begin(), tokens.end(), (*iter)), tokens.end());
  
  }

}


void Tokenize(const string& str, vector<string>& tokens, const string& delimiters = " "){
  string::size_type lastPos = str.find_first_not_of(delimiters, 0);
  string::size_type pos = str.find_first_of(delimiters, lastPos);

  while (string::npos != pos || string::npos != lastPos){
    string a = str.substr(lastPos, pos-lastPos);
    makeLower(a); // Converting into lowercase characters.
    remove_punctuations(a);
    if(std::find(tokens.begin(), tokens.end(), a) == tokens.end()) // find() returns tokens.end if tarket is not found. 
      tokens.push_back(a);
    lastPos = str.find_first_not_of(delimiters, pos);
    pos = str.find_first_of(delimiters, lastPos);
  }
  remove_stopwords(tokens);

}

void initWeight(double who[nnum][vnum], double wih[vnum][nnum]){
  for(int i=0;i<nnum;i++){
    for(int j=0;j<vnum;j++){
      who[i][j] = 1.0;
      wih[j][i] = 1.0;
    }
  }

}

double activation(double x){
  return 1.0/(1+exp(-x));
}

int main()
{
  //int vnum = 5;
  //int nnum = 2;
  char inputString[MAX_STRING];
  
  string buf;

  vector<string> tokens;

  double temp;
  int number;

  int data[20] = {1, 1, 3, 3, 1, 1, 3, 1, 1, 3, 4, 1, 3, 3, 1, 1, 1, 3, 1, 3};
  int answer[20] = {2, 2, 4, 4, 2, 2, 4, 4, 2, 4, 1, 2, 4, 4, 2, 2, 2, 4, 2, 4};

  int x[vnum]; // Input nodes
  double h[nnum]; // Hidden nodes
  double y[vnum]; // Output nodes
  int t[vnum]; // Right Answers!
  
  double berror[nnum];
  
  double who[nnum][vnum]; // Weight matrix between hidden layer and output layer
  double wih[vnum][nnum]; // Weight matrix between input layer and hidden layer

  double eta = 0.1; // Learning rate ( > 0)
  
  ifstream inFile("input.txt");
  ofstream outFile("outfile.txt");

  while(!inFile.eof()){
    inFile.getline(inputString,MAX_STRING);
    Tokenize(inputString, tokens);
  }

  for(vector<string>::size_type i = 0; i < tokens.size(); ++i)
    outFile << tokens[i] << endl;
  
  initWeight(who,wih); // Initialize the weight matrix

  
  // Iteration START
  number = 0;
  while(number<20){
  // Begin of Feedforwarding

  for(int i=0;i<vnum;i++){
  switch(data[number])
  {
    case 1:
      x[0] = 1;x[1] = 0; x[2] = 0; x[3] = 0;
      break;
    case 2:
      x[0] = 0;x[1] = 1; x[2] = 0; x[3] = 0;
      break;
    case 3:
      x[0] = 0;x[1] = 0; x[2] = 1; x[3] = 0;
      break;
    case 4:
      x[0] = 0;x[1] = 0; x[2] = 0; x[3] = 1;
      break;
  }  
  }

  for(int i=0;i<vnum;i++){
  switch(answer[number])
  {
    case 1:
      t[0] = 1;t[1] = 0; t[2] = 0; t[3] = 0;
      break;
    case 2:
      t[0] = 0;t[1] = 1; t[2] = 0; t[3] = 0;
      break;
    case 3:
      t[0] = 0;t[1] = 0; t[2] = 1; t[3] = 0;
      break;
    case 4:
      t[0] = 0;t[1] = 0; t[2] = 0; t[3] = 1;
      break;
  }  
  }
  

  for(int i=0;i<nnum;i++){
    temp = 0.0;
    for(int j=0;j<vnum;j++){  
      temp += wih[j][i]*x[j];
    }
    h[i] = activation(temp);
  }

  for(int i=0;i<vnum;i++){
    temp = 0.0;
    for(int j=0;j<nnum;j++){
      temp += who[i][j]*h[j];
    }
    y[i] = activation(temp);
  }

  // End of Feedforwarding

  // Begin of Backpropagation
  for(int i=0;i<nnum;i++){
    for(int j=0;j<vnum;j++){
      who[i][j] = who[i][j] - eta * (y[j]-t[j])*h[i];
    }
  }

  for(int i=0;i<vnum;i++){
    for(int j=0;j<nnum;j++){
      
      for(int q=0;q<nnum;q++){
	berror[q] = 0.0;
	for(int p=0;p<vnum;p++){
	  berror[q] += (y[p] - t[p])*who[q][p];
	}
      }

      wih[i][j] = wih[i][j] - eta * berror[j]*x[i];
    }
  }
   // End of Backpropagation
  number++;
  }
  // Iteration END

  inFile.close();
  outFile.close();

}
