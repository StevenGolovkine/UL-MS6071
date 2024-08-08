################################################################################
# Script file for Simulations
################################################################################


set.seed(42)
runif(4)
runif(4)

set.seed(42)
runif(4)

set.seed(42)               # seed setting
theta <- 4; n <- 10000     # parameters setting
u <- runif(n)              # uniform sample
x <- -log(1 - u) / theta   # inverse transform method

ks.test(x, "pexp", theta)  # Kolmogorov Smirnov test

set.seed(10)              # seed setting
theta <- 0.4; n <- 10000  # parameters setting
u <- runif(n)             # uniform sampling
x <- (u > (1 - theta))    # inverse transform method

chisq.test(table(x), p = c(1 - theta, theta))  # chisq test


library(tidyverse)

set.seed(42)
rnorm(5)

set.seed(42)
rbinom(5, 10, 0.5)

set.seed(42)
sample <- rbeta(1000, 2, 2)  # Generate B(2, 2)

density_estim <- density(sample, from = 0, to = 1)
density_true <- dbeta(density_estim$x, 2, 2)

density_df <- tibble(
  x = density_estim$x,
  True = density_true,
  Estimation = density_estim$y
) |> 
  pivot_longer(cols = c(True, Estimation))

ggplot(density_df) +
  geom_line(aes(x = x, y = value, color = name)) +
  xlab("") +
  ylab("Density") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.9, 0.9)
  )

set.seed(42)
sample <- rpois(1000, 5)  # Generate P(5)

x = seq(-1, 20, by = 0.1)
ecdf_estim <- ecdf(sample)(x)
ecdf_true <- ppois(x, 5)

ecdf_df <- tibble(
  x = x,
  True = ecdf_true,
  Estimation = ecdf_estim
) |> 
  pivot_longer(cols = c(True, Estimation))

ggplot(ecdf_df) +
  geom_step(
    aes(x = x, y = value, color = name),
    direction = "hv"
  ) +
  geom_point(
    aes(x = x, y = value, color = name),
    data = filter(ecdf_df, x %in% seq(-1, 20, by = 1))
  ) +
  xlab("") +
  ylab("ECDF") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.9, 0.1)
  )



library(tidyverse)

set.seed(42)
n <- 100
x <- runif(n, 30, 70)
eps <- rnorm(n, 0, 1)
y <- 9 + 0.25 * x + eps

ggplot() +
  geom_point(aes(x = x, y = y)) +
  xlab("Circumference") +
  ylab("Height") +
  theme_bw()


set.seed(42)
n <- 1000
x <- runif(n, -1, 1)
eps <- rnorm(n, 0, 0.25)
mu <- 0.5 + 0.3 * x + eps
y <- rpois(n, exp(mu))

ggplot() +
  geom_point(aes(x = x, y = y)) +
  xlab("") +
  ylab("") +
  theme_bw()

glm(y ~ x, family = poisson(link = "log"))


library(tidyverse)

set.seed(42)  # seed setting
n <- 10000  # number of samples   
probs <- c(0.5, 0.25, 0.25)  # probabilities to be in each mixture

# Generate the pi variable and each mixture separetely
pi <- sample(c(1, 2, 3), n, replace = TRUE, prob = probs)
f1 <- rnorm(n)
f2 <- rnorm(n, 2, 1.5)
f3 <- rnorm(n, 4, 1.5)

# Create the mixture
f <- (pi == 1) * f1 + (pi == 2) * f2 + (pi == 3) * f3

set.seed(42)

density_estim <- density(f, from = -5, to = 10)
density_true <- (
    0.5 * dnorm(density_estim$x) +
    0.25 * dnorm(density_estim$x, 2, 1.5) +
    0.25 * dnorm(density_estim$x, 4, 1.5)
)

density_df <- tibble(
  x = density_estim$x,
  True = density_true,
  Estimation = density_estim$y
) |> 
  pivot_longer(cols = c(True, Estimation))

ggplot(density_df) +
  geom_line(aes(x = x, y = value, color = name)) +
  xlab("") +
  ylab("Density") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.9, 0.9)
  )

set.seed(42)  # seed setting
n <- 10000  # number of samples 
probs <- c(0.5, 0.5)  # probabilities to be in each mixture

# Generate the pi variable and each mixture separetely
pi <- sample(c(1, 2), n, replace = TRUE, prob = probs)
f1 <- rpois(n, 5)

# Create the mixture
f <- (pi == 1) * 0 + (pi == 2) * f1


# Compute the true probabilities and an estimation
prob_estim <- as.vector(table(f) / n)[1:15]
prob_true <- 0.5 + 0.5 * exp(-5)
for (i in 1:14) {
    prob_true <- c(prob_true, 0.5 * 5^i * exp(-5) / factorial(i))
}

probs_df <- tibble(
    x = seq(0, 14, by = 1),
    True = prob_true,
    Estimation = prob_estim
) |> 
    pivot_longer(cols = c(True, Estimation))

ggplot(probs_df) +
    geom_col(
        aes(x = x, y = value, fill = name),
        position = "dodge"
    ) +
    xlab("") +
    ylab("Probability") +
    theme_bw() +
    theme(
        legend.title = element_blank(),
        legend.position = c(0.9, 0.9)
    )



