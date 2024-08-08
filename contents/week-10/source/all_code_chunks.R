################################################################################
# Script file for C++
################################################################################

# Don't run it.

library(Rcpp)

#include <Rcpp.h>  // To use Rcpp functions
using namespace Rcpp;  // To add Rcpp functions in the namespace

// [[Rcpp::export]]  // To make the function available in R
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}

/*** R
timesTwo(42)
*/

function (x) 
.Call(<pointer: 0x106c96aec>, x)

one <- function() 1L

int one() {
  return 1;
}

#include <algorithm>


LinkingTo: Rcpp
Imports: Rcpp

useDynLib(mypackage)
importFrom(Rcpp, sourceCpp)


#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

X = matrix(rnorm(100), nrow = 10, ncol = 10)  # Create a matrix
X[1, 1]  # Slicing
nrow(X)  # Get the number of rows
t(X)  # Transpose the matrix

mat X = mat(10, 10, arma::fill::randn);  // Create a matrix
X(0, 0)  // Slicing
X.n_rows  // Get the number of rows
X.t()  // Transpose the matrix

mat X(100, 5, fill::randu);  // Matrix with Gaussian distribution

mat coeff, score;  // Create placeholders
vec latent, tsquared;  // Create placeholders
princomp(coeff, score, latent, tsquared, X);  // Perforn PCA

mat means;  // Create placeholders
bool status = kmeans(means, X, 2, random_subset, 10, true);  // KMeans


