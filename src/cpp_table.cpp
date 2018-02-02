#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix cpp_table(NumericVector x, NumericVector y, NumericVector w) {
  
  // create vectors of unique x and y values
  NumericVector ux = Rcpp::sort_unique(x);
  NumericVector uy = Rcpp::sort_unique(y);
  
  // create maps of indices to the values
  std::map <double, int> xmap;
  std::map <double, int> ymap;
  
  // populate the maps
  for (int i = 0; i < ux.size(); i++){
    xmap[ux[i]] = i;
  }
  
  for (int j = 0; j < uy.size(); j++){
    ymap[uy[j]] = j;
  }
  
  // create the return matrix
  int len = x.size();
  NumericMatrix mm(ux.size(), uy.size());
  
  // iterate over the length of the vector and fill the matrix
  for(int i = 0; i < len; i++) {
    mm(xmap[x[i]], ymap[y[i]]) += w[i];
  }
  
  return mm;
}