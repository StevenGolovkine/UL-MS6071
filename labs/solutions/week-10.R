################################################################################
# Possible solution for lab - Memory Issues
################################################################################

# Load C++ functions
Rcpp::sourceCpp("./week-10.cpp")

# Linear regression
my_lm <- function(x, y) {
    xbar <- mean(x)
    ybar <- mean(y)
    x2bar <- mean(x * x)
    y2bar <- mean(y * y)
    xybar <- mean(x * y)
    
    beta1hat <- (xybar - xbar * ybar) / (x2bar - xbar * xbar)
    beta0hat <- ybar - beta1hat * xbar
    sigmahat <- sqrt(
        length(x) * (
            y2bar + beta0hat * beta0hat + beta1hat * beta1hat * x2bar
            - 2 * beta0hat * ybar
            - 2 * beta1hat * xybar
            + 2 * beta0hat * beta1hat * xbar
        ) / (length(x) - 2)
    )
    sigbeta1hat <- sigmahat / sqrt(length(x) * (x2bar - xbar * xbar))
    
    result <- list(
        xbar = xbar,
        ybar = ybar,
        x2bar = x2bar,
        y2bar = y2bar,
        xybar = xybar,
        beta0hat = beta0hat,
        beta1hat = beta1hat,
        sigmahat = sigmahat,
        sigbeta1hat = sigbeta1hat
    )
    return(result)
}

# Monte-Carlo procedure
my_monte_carlo <- function(
    x = 1:10,
    M = 1e6,
    beta0 = 3,
    alpha = 0.05,
    rand = runif,
    ...
) {
    d <- 0
    for (i in 1:M) {
        eps <- rand(length(x), ...)
        y <- beta0 + eps
        model <- my_lm(x, y)
        t_obs <- model$beta1hat / model$sigbeta1hat
        quantile <- qt(p = 1 - alpha/2, df = length(x) - 2)
        if (abs(t_obs) > quantile) {
            d <- d + 1
        }
    }
    return(d / M)
}

monte_carlo_lm <- function(
    x = 1:10,
    M = 1e6,
    beta0 = 3,
    alpha = 0.05,
    rand = runif,
    ...
) {
    d <- 0
    for (i in 1:M) {
        eps <- rand(length(x), ...)
        y <- beta0 + eps
        model <- summary(lm(y ~ x))
        t_obs <- model$coefficient[2, 1] / model$coefficient[2, 2]
        quantile <- qt(p = 1 - alpha / 2, df = length(x) - 2)
        if (abs(t_obs) > quantile) {
            d <- d + 1
        }
    }
    return(d / M)
}


# Comparison
bench::mark(
    my_monte_carlo(x = 1:10, M = 1e3, beta0 = 3, alpha = 0.05),
    monte_carlo_lm(x = 1:10, M = 1e3, beta0 = 3, alpha = 0.05),
    monte_carlo_c(xvec = 1:10, M = 1e3, beta0 = 3, alpha = 0.05),
    check = FALSE
)

# The results are expected. The fastest function is the C++ one. Using the
# function that is implemented in R takes more time, because it performs
# unnecessary operations in the function (inputs checking, building a matrix to
# work with more features, performing more test, storing results, ...).
