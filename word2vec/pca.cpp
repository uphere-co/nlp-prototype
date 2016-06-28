#include "pca.h"
#include <iostream>
#include <random>

#include <boost/tuple/tuple.hpp>

#include "gnuplot-iostream.h"

using namespace std;

int main() {

  const int num_variables = 100; // Dimensions of word vector
  const int num_records = 300; // Number of vocabularies
  
  stats::pca pca(num_variables);
  pca.set_do_bootstrap(true, 100);

  for (int i=0; i<num_records; ++i) {
    vector<double> record(num_variables);
    for (auto value=record.begin(); value!=record.end(); ++value)
      *value = rand()%20 - 10;

    pca.add_record(record);
  }

  cout<<"Solving ..."<<endl;
  pca.solve();

  cout<<"Energy = "<<pca.get_energy()<<" ("<<
    stats::utils::get_sigma(pca.get_energy_boot())<<")"<<endl;

  const auto eigenvalues = pca.get_eigenvalues();
  cout<<"First three eigenvalues = "<<eigenvalues[0]<<", "
      <<eigenvalues[1]<<", "
      <<eigenvalues[2]<<endl;

  cout<<"Orthogonal Check = "<<pca.check_eigenvectors_orthogonal()<<endl;
  cout<<"Projection Check = "<<pca.check_projection_accurate()<<endl;

  pca.save("pca_results");

  const vector<double> principal_1 = pca.get_principal(0);
  const vector<double> principal_2 = pca.get_principal(1);

  for(int i = 0 ; i < principal_1.size(); i++)
    cout << principal_1[i] << "  ";
  cout << endl;

  for(int i = 0 ; i < principal_2.size(); i++)
    cout << principal_2[i] << "  ";
  cout << endl;
  
  // Very simple use case of gnuplot
  Gnuplot gp;

  vector<pair<double, double> > xy_pts_A;
  vector<pair<double, double> > xy_pts_B;

  for(double x = -2; x <2; x += 0.01) {
    double y = x*x*x;
    xy_pts_A.push_back(make_pair(x, y));
  }

  for(double alpha = 0; alpha < 1; alpha += 1.0/24.0) {
    double theta = alpha*2.0*3.14159;
    xy_pts_B.push_back(make_pair(cos(theta), sin(theta)));
  }

  gp << "set xrange [-2:2]\nset yrange [-2:2]\n";
  gp << "plot" << gp.file1d(xy_pts_A) << "with lines title 'cubic',"
     << gp.file1d(xy_pts_B) << "with points title 'circle'" << endl;
  
  
  //
  

  
  return 0;
}
