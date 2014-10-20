#include <Rcpp.h>
#include <cmath> 
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::NumericMatrix tableCPP(Rcpp::NumericVector x, Rcpp::NumericVector y, 
					  Rcpp::NumericVector k)
{
  Rcpp::NumericMatrix d(k[0], k[0]);
  int n = x.size();
  if (y.size() != n)
  {
    Rcpp::Rcout << "Error: the size of x and y must be the same.\n";
    // Fix here
    return 0;
  }
  for (int i = 0; i < n; i++) d(x[i]-1,y[i]-1) += 1;
  return d;
}