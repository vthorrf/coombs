// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// [[Rcpp::export]]
arma::vec pascalTriangle(int h) {
    arma::vec s(h);
    for (int j = 0; j < h; j++) {
        s[j] = Rf_choose(h - 1, j);
    }
    return s;
}