#include <Rcpp.h>
#include <cmath> 
using namespace Rcpp;

// [[Rcpp::export]]
double L1Dist(Rcpp::NumericVector x, Rcpp::NumericVector y)
{
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
