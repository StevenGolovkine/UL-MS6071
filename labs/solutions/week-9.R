################################################################################
# Possible solution for lab - Debugging and profiling
################################################################################

# 1. In this function, we want to compute $n!$ for $n \in \mathbb{N}$. For the
# purpose of the exercise, do not use the built-in `factorial` function.

calc_factorial <- function(n) {
    if (n < 0) {
        print("n cannot be negative.")
        return (NA)
    } else if (n == 0){
        return (0)
    } else {
        return (prod(1:n))
    }
}

calc_factorial(10)
calc_factorial(100)
calc_factorial(0)
calc_factorial(-5)

# 2. Here, we would like to determine if $n$ is prime and then, compute the sum
# of the prime until $N$.

is_prime <- function(n) {
    if (n <= 1) {
        return(FALSE)
    }
    if (n <= 3) {
        return(TRUE)
    }
    if (n %% 2 == 0 || n %% 3 == 0) {
        return(FALSE)
    }
    i <- 5
    while (i * i <= n) {
        if (n %% i == 0 || n %% (i + 2) == 0) {
            return(FALSE)
        }
        i <- i + 6
    }
    return(TRUE)
}

is_prime(10)
is_prime(13)
is_prime(1147851)
is_prime(200560490131)

# 3. Using a list of names, with associated ages, find the age associated to
# the name.
find_age <- function(name, names, ages) {
    ages[names == name]
}

names <- c("Alice", "Bob", "Charlie", "David", "Eve")
ages <- c(25, 32, 28, 45, 30)
find_age("Charlie", names, ages)

set.seed(42)
names <- replicate(
    1e6, paste(sample(LETTERS, size = 3, replace = TRUE), collapse = "")
)
ages <- sample(20:95, 1e6, replace = TRUE)
find_age("WOZ", names, ages)


# 4. In a dataframe, we would like to retrieve get all the value larger than $1$
# for a specific column.
subset_data <- function(dataset) {
    library(tidyverse, warn.conflicts = FALSE)
    dataset |> filter(value > 1) |> pull(value)
}

df <- data.frame(id = 1:1e2, value = rnorm(1e2))
v <- subset_data(df)

df <- data.frame(id = 1:1e6, value = rnorm(1e6))
v <- subset_data(df)
