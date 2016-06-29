#include <iostream>
#include <random>

#include <boost/tuple/tuple.hpp>

#include "pca.h"
#include "utils.h"

#include "gnuplot-iostream.h"

std::string word_vector_file;

// main function arguments

void printHelp() {
  std::cout << "c++ pca implementation \n\n";
  std::cout << "Options:\n";
  std::cout << "Parameters for drawing:\n";
  std::cout << "\t-word-vector <file>\n";
  std::cout << "\t\tUse word vector stored in <file>\n";
  std::cout << "\nExamples:\n";
  std::cout << "./pca -word-vector data.txt";
  
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
}

// main function arguments

int main(int argc, char **argv) {

  std::string line;
  std::vector<std::string> word;

  std::vector< std::vector<double> > data;
  std::vector<std::string> label;
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

  getline(inFile, line);
  split(line, word);
  //int num_records = atoi(word[0].c_str());
  int num_records = 30;
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

  */
  std::cout<<"Orthogonal Check = "<<pca.check_eigenvectors_orthogonal()<<std::endl;;
  std::cout<<"Projection Check = "<<pca.check_projection_accurate()<<std::endl;;
  


  //pca.save("pca_results");

  std::vector<double> principal_1 = pca.get_principal(0);
  std::vector<double> principal_2 = pca.get_principal(1);

  double prin1[principal_1.size()];
  double prin2[principal_2.size()];

  
  for(int i = 0 ; i < principal_1.size(); i++) {
    prin1[i] = principal_1[i];
    prin2[i] = principal_2[i];
  }

  /* 
  for(int i = 0 ; i < principal_2.size(); i++)
    std::cout << principal_2[i] << "  ";
  std::cout << std::endl;
  */  
  
  // Very simple use case of gnuplot
  Gnuplot gp; // this does something really really strange
  
  //std::vector< std::pair< std::string, std::pair<double, double> > > xy_pts_A;
  std::vector< boost::tuple<std::string, double, double> > xy_pts_A;
 
  double x, y;

  std::vector<double> tempx;
  for(int i = 0; i < num_records; i++) {
    x = 0;
    for(int j = 0; j < num_variables; j++) {
      x += prin1[j] * data[i][j];
    }
    tempx.push_back(x);
  }

  std::vector<double> tempy;
  for(int i = 0; i < num_records; i++) {
    y = 0;
    for(int j = 0; j < num_variables; j++) {
      y += prin2[j] * data[i][j];
    }
    tempy.push_back(y);
  }

  for(int i = 0; i < num_records; i++) {
    xy_pts_A.push_back(boost::make_tuple(label[i],tempx[i],tempy[i]));
  }


  
  /*
  for(double x = -2; x <2; x += 0.01) {
    double y = x*x*x;
    xy_pts_A.push_back(std::make_pair(x, y));
    }*/
  /*
  for(double alpha = 0; alpha < 1; alpha += 1.0/24.0) {
    double theta = alpha*2.0*3.14159;
    xy_pts_B.push_back(std::make_pair(cos(theta), sin(theta)));
  }
  */
  
  //gp << "set xrange [-100:100]\nset yrange [-100:100]\n";



  gp << "plot" << gp.file1d(xy_pts_A, "temp.dat") << " using 2:3:1 with labels offset 1 title 'word vectors'" << std::endl;
  gp.send1d(xy_pts_A);

  
  //gp << "plot" << gp.file1d(xy_pts_A, "temp.dat") << "using 2:3, " << gp.file1d(xy_pts_A, "temp2.dat") << " 2:3:1 with labels offset 1 title 'word vectors'," << std::endl;
    //<< gp.file1d(xy_pts_B) << "with points title 'circle'" << std::endl;
  
  return 0;
}
