#include <armadillo>
#include <iostream>

void testLinearity(std::vector<arma::fvec>& vectors){
arma::fmat target_matrix(vectors[0].size(),vectors.size());
  for(int i = 0; i < vectors.size(); i++) {
    target_matrix.col(i) = vectors[i];
}

target_matrix.print();
std::cout << rank(target_matrix) << std::endl;
}

double dotProduct(arma::fvec vec1, arma::fvec vec2)
{
  double result = 0;
  if(vec1.size() != vec2.size()) {
    std::cout << "Wrong dot product operation!";
    exit(1);
  }
  else {
    for(int i = 0; i < vec1.size(); i++) {
      result += vec1[i] * vec2[i];
    }
  }

  return result;
}

arma::fvec projectionOp(arma::fvec vec1, arma::fvec vec2) {
  double num = dotProduct(vec1, vec2);
  double den = dotProduct(vec1, vec1);

  return (num)/(den) * vec1;
}

arma::fvec sumprojections(std::vector<arma::fvec> vectors, std::vector<arma::fvec> r_vectors, int k) {
  arma::fvec r_vector(vectors[0].size());
  r_vector.zeros();
  
  for(int i = 0; i < k; i++) {
    r_vector += projectionOp(r_vectors[i],vectors[k]);
  }

  return r_vector;
}

std::vector<arma::fvec> makeBasis(std::vector<arma::fvec> vectors) {
  std::vector<arma::fvec> r_vectors;
  r_vectors.reserve(vectors.size());
  for(int i = 0; i < vectors.size(); i++) {
    r_vectors.push_back(vectors[i] - sumprojections(vectors, r_vectors, i));
  }
  
  return r_vectors;
}

void makeNormal(std::vector<arma::fvec>& vectors) {
  for(int i = 0; i < vectors.size(); i++) {
    vectors[i] = arma::normalise(vectors[i]);
  }
}

int main() {
  
  //fvec column vector
  
  arma::fmat inMat;
  arma::fvec v1;
  arma::fvec v2;
  arma::fvec v3;
  
  v1 = {1, 2, 3};
  v2 = {2, 3, 4};
  v3 = {3, 4, 9};
  
  std::vector<arma::fvec> v;
  std::vector<arma::fvec> rv;
  v.push_back(v1);
  v.push_back(v2);
  v.push_back(v3);

  rv = makeBasis(v);
  makeNormal(rv);
  for(int i = 0; i < rv.size(); i++) {
    rv[i].print();
  }

  inMat = {{1,2,3},{2,3,4},{3,4,9}};
  std::cout << rank(inMat) << std::endl;
  
  //arma::fmat U;
  //arma::fvec S;
  //arma::fmat V;
  

  //arma::svd(U, S, V, inMat);
  
  //inMat.print("inMat = ");
  //U.print("U = ");
  //S.print("S = ");
  //V.print("V = ");
  
  return 0;
}
