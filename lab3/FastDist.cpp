#include <Rcpp.h>
#include <cmath> 
#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;
using Eigen::Map;                   // 'maps' rather than copies 
using Eigen::MatrixXd;                  // variable size matrix, double precision
using Eigen::VectorXd;                  // variable size vector, double precision
using Eigen::SelfAdjointEigenSolver;


// [[Rcpp::export]]
VectorXd getEigenValuesEG(Map<MatrixXd> M) {
    SelfAdjointEigenSolver<MatrixXd> es(M);
        return es.eigenvalues();
        }



// [[Rcpp::export]]
double L1Dist(Rcpp::NumericVector x, Rcpp::NumericVector y)
{
	// This function return the L1 distance normalized by n
  double result = 0.0;
  int n = x.size();
  if (y.size() != n)
  {
    Rcpp::Rcout << "Error: the size of x and y must be the same.\n";
    // Fix here
    return 0;
  }

  for (int i = 0; i < n; i++)
  {
    result += std::abs(x[i] - y[i]);
  }
  return result/n;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix Distance(Rcpp::NumericMatrix x)
{
	// This function return the L1 distance matrix of n points
	// in d dimensional space (x has dimension n*d)
  int n = x.nrow(); 
  Rcpp::NumericMatrix d(x.nrow(), x.nrow());
  for (int i = 0; i < n; i++)
  {
    for (int j = 0; j < i; j++)
    {
      d(i,j) = d(j,i) = L1Dist(x(i,_), x(j,_));
    }
    d(i,i) = 0;
  }
  return d;
}


