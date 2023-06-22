
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector pascalTriangle(int h) {
    NumericVector s(h);
    for (int j = 0; j < h; j++) {
        s[j] = Rf_choose(h - 1, j);
    }
    return s;
}
