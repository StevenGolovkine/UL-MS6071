#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;


// [[Rcpp::export]]
List lm_c(
    NumericVector x,
    NumericVector y
) {

    int i, n = x.size();
    double xybar = 0.0, xbar = 0.0, ybar = 0.0, x2bar = 0.0, y2bar = 0.0;
    double sumx = 0.0, sumy = 0.0, sumxy = 0.0, sumx2 = 0.0, sumy2 = 0.0;
    double beta1hat, beta0hat, sigmahat, sigbeta1hat;
    
    xbar = mean(x);
    ybar = mean(y);
    x2bar = mean(x * x);
    y2bar = mean(y * y);
    xybar = mean(x * y);
    
    beta1hat = (xybar - xbar * ybar) / (x2bar - xbar * xbar);
    beta0hat = ybar - beta1hat * xbar;
    
    sigmahat = sqrt(
        (double)n * (y2bar
            + beta0hat * beta0hat
            + beta1hat * beta1hat * x2bar
            - 2 * beta0hat * ybar
            - 2 * beta1hat * xybar
            + 2 * beta0hat * beta1hat * xbar
        ) / ((double)n - 2)
    );
    sigbeta1hat = sigmahat / sqrt(n * (x2bar - xbar * xbar));
        
    
    return List::create(
        _["xbar"] = xbar,
        _["ybar"] = ybar,
        _["x2bar"] = x2bar,
        _["y2bar"] = y2bar,
        _["xybar"] = xybar,
        _["beta0hat"] = beta0hat,
        _["beta1hat"] = beta1hat,
        _["sigmahat"] = sigmahat,
        _["sigbeta1hat"] = sigbeta1hat
    );
} // lm_C

// [[Rcpp::export]]
NumericVector monte_carlo_c(
    NumericVector xvec,
    int M,
    double beta0,
    double alpha
) {
    int d = 0, m, i, n = xvec.size();
    double thresh, tobs;
    List results;
    NumericVector yvec(n);
    thresh = R::qt(1.0 - alpha/2.0, (double)(n - 2), 1, 0);
    GetRNGstate();
    for (m = 0; m < M; m++) {
        yvec = beta0 + Rcpp::runif(n, 0.0, 1.0);
        results = lm_c(xvec, yvec);
        tobs = (double)results["beta1hat"] / (double)results["sigbeta1hat"];
        if (fabs(tobs) > thresh) d = d + 1;
    }
    PutRNGstate();
    return Rcpp::wrap((double)d / (double)M);
} // monte_carlo_c

