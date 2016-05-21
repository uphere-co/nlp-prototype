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

void Tokenize(const string& str, vector<string>& tokens, const string& delimiters = " "){
  string::size_type lastPos = str.find_first_not_of(delimiters, 0);
  string::size_type pos = str.find_first_of(delimiters, lastPos);

  while (string::npos != pos || string::npos != lastPos){
    tokens.push_back(str.substr(lastPos, pos - lastPos));
    lastPos = str.find_first_not_of(delimiters, pos);
    pos = str.find_first_of(delimiters, lastPos);
  }
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
  char inputString[1000];

  //int vnum = 5;
  //int nnum = 2;
  
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
  
//ifstream inFile("input.txt");
//  ofstream outFile("outfile.txt");

//while(!inFile.eof()){
//    inFile.getline(inputString,1000);
//    Tokenize(inputString, tokens);
//  }

//  for(vector<string>::size_type i = 0; i < tokens.size(); ++i)
//    outFile << tokens[i] << endl;
  
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

  //inFile.close();
  //outFile.close();

}
