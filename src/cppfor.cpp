#include <Rcpp.h>
using namespace Rcpp;

//' @name cppfor
//' @title Fast Implementation of Dynamic Algorithm Using C++
//'
//' @description This function creates an equivalent of the nested for loop in
//' the dynamic algorithm for a faster implementation using C++ code instead of
//' R code.
//'
//' @param n The number of available objects to place in the knapsack
//' @param w The numeric weight limit of the knapsack
//' @param w A numeric vector which contains the weights from data.frame 'x'
//' @param v A numeric vector which contains the values from data.frame 'x'
//' @param mat A zero matrix with (n + 1) rows and (W + 1) columns, with n
//' being the number of available objects and W being the weight limit of the
//' knapsack
//' @param mem A zero data.frame with one column and (n + 1) rows
//'
//'

// [[Rcpp::export]]

DataFrame cppfor (int n, int W, NumericVector w, NumericVector v, NumericMatrix mat, DataFrame mem) {

  NumericVector m = {0};

  for (int i = 1; i <= n; i++) {
    for (int j = 0; j <= W; j++) {

      if(w[i] > j) {
        mat(i, j) = mat((i - 1), j);

      } else {
        mat(i, j) = std::max(mat((i - 1), j), (mat((i - 1), (j - w[i])) + v[i]));

        if (i != n && std::find(m.begin(), m.end(), mat(i, j)) == m.end()){
          double prevval = mat((i - 1), (j - w[i]));

          double * match = std::find(m.begin(), m.end(), prevval);
          int previndex = match - m.begin();

          NumericVector prevvec = mem[previndex];
          NumericVector addmemrow = clone(prevvec);
          addmemrow[0] = mat(i, j);
          addmemrow[(i + 1)] = 1;

          mem.push_back(addmemrow);
          m.push_back(mat(i, j));
        }
      }
    }
  }
  return mem;}


