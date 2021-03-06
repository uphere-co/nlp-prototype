#include <iostream>
#include <random>

#include <boost/tuple/tuple.hpp>

#include "pca.h"
#include "utils.h"

std::string word_vector_file;
std::vector<std::string> search_words;

// main function arguments

void printHelp() {
  std::cout << "c++ pca implementation \n\n";
  std::cout << "Options:\n";
  std::cout << "Parameters for drawing:\n";
  std::cout << "\t-word-vector <file>\n";
  std::cout << "\t\tUse word vector stored in <file>\n";
  std::cout << "\t-draw-words <words>\n";
  std::cout << "\t\tChoose <words> to be drawed\n";
  std::cout << "\nExamples:\n";
  std::cout << "./pca -word-vector data.txt -draw-words king queen prince princess\n";
  
}

int ArgPos(char *str, int argc, char **argv) {
  int i;
  std::string s_str;
  std::string s_argv[argc];
  
  for(i = 1; i < argc; i++) s_argv[i] = argv[i];
  s_str = str;
  
  for(i = 1; i < argc; i++) if(s_str == s_argv[i]) {
      if(i == argc - 1) {
	std::cout << "Argument missing for " << s_str << std::endl;;
	exit(1);
      }
      return i;
  }
  return -1;
}

void ArgPass(int argc, char **argv) {
  int i;
  if ((i = ArgPos((char *)"-word-vector", argc, argv)) > 0) word_vector_file = argv[i + 1];
  if ((i = ArgPos((char *)"-draw-words", argc, argv)) > 0) {
    for(int j = 0; j < argc - 4; j++)
      {
	search_words.push_back(argv[i + 1 + j]);
      }
  }
}

// main function arguments

int main(int argc, char **argv) {
  std::ofstream fout;
  
  std::string line;
  std::vector<std::string> word;

  std::vector< std::vector<double> > data;
  std::vector<std::string> label;

  fout.open("temp.dat", std::ofstream::out);
  
  if(argc == 1) {
    printHelp();
    return 0;
  }

  ArgPass(argc, argv);

  std::ifstream inFile;
  inFile.open(word_vector_file, std::ifstream::in);
  if(inFile.fail()) {
    std::cout << "Word vector file not found!\n";
    exit(1);
  }

  std::getline(inFile, line);
  split(line, word);
  int num_records = atoi(word[0].c_str());
  int num_variables = atoi(word[1].c_str());
  stats::pca pca(num_variables);
  pca.set_do_bootstrap(true, 100);

  for(int i = 0; i < num_records; ++i) {
    word.clear();
    std::getline(inFile,line);
    split(line, word);
    std::vector<double> record(num_variables);
    for(int j = 0; j < num_variables; j++) record[j] = atof(word[j+1].c_str());
    
    pca.add_record(record);
    label.push_back(word[0]);
    data.push_back(record);
  }

  
  pca.solve();

  /*
  std::cout<<"Energy = "<<pca.get_energy()<<" ("<<
    stats::utils::get_sigma(pca.get_energy_boot())<<")"<<std::endl;;

  const auto eigenvalues = pca.get_eigenvalues();
  std::cout<<"First three eigenvalues = "<<eigenvalues[0]<<", "
      <<eigenvalues[1]<<", "
	   <<eigenvalues[2]<<std::endl;;

  
  std::cout<<"Orthogonal Check = "<<pca.check_eigenvectors_orthogonal()<<std::endl;;
  std::cout<<"Projection Check = "<<pca.check_projection_accurate()<<std::endl;;
  */
  
  //pca.save("pca_results");

  std::vector<double> principal_1 = pca.get_principal(0);
  std::vector<double> principal_2 = pca.get_principal(1);

  // Very simple use case of gnuplot
  
  std::vector< boost::tuple<std::string, double, double> > xy_pts_A;

  
  double x, y;

  for(int i = 0; i < num_records; i++) {
    x = 0;
    y = 0;
    for(int j = 0; j < num_variables; j++) {
      x += principal_1[j] * data[i][j];
      y += principal_2[j] * data[i][j];
    }
    //xy_pts_A.push_back(boost::make_tuple(label[i],x,y));
  }

  for(int i = 0; i < num_records; i++) {
    for(auto& k: search_words) {
      if( label[i] == (k) ) {
	x = 0;
	y = 0;
	for(int k = 0; k < num_variables; k++) {
	  x += principal_1[k] * data[i][k];
	  y += principal_2[k] * data[i][k];
	}
	xy_pts_A.push_back(boost::make_tuple(label[i],x,y));
      }
    }
  }
  
  for(const auto& i : xy_pts_A) {
    fout << boost::get<0>(i) << " " << boost::get<1>(i)<< " " << boost::get<2>(i) << std::endl;
  }

  fout.open("script.gp",std::ofstream::out);

  fout << "set term png" << std::endl;
  fout << "set output \"sample.png\"" << std::endl;
  fout << "plot 'temp.dat' using 2:3:1 with labels offset 1 title 'word vectors'" << std::endl;

  fout.close();
  return 0;
}
