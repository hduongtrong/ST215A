#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
using namespace arma;
vec getEigenValuesARMA(mat M) 
  {
    vec eigval;
    mat eigvec;
    eig_sym(eigval, eigvec, M);
    return eigval;
  }
