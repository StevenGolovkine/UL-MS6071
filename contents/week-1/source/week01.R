################################################################################
# Script file for Week 01 - R Basics
################################################################################

# R Basics ----

# This is a comment.
# And, here another one!

x <- 1  # Nothing is printed...

10 + 4 / 2

(10 + 4) / 2

x <- 1
# ----

# Types and Classes in R ----

double_vector <- c(1, 2, 3)
integer_vector <- c(1L, 2L, 3L)  # Note the difference with the previous vector.
character_vector <- c("apple", "banana", "peach")

l <- list(1, FALSE, "hello")

l <- list(
    a = c(1, 2, 3),
    b = "hello",
    c = c(TRUE, FALSE)
)
# ----

# Logical Operators ----

x <- c(1, 3.5, 7.9, 10, 12.3, NA, 15.8)

x == 3.5

x != 3.5

x > 3.5

!(x > 3.5)

x > 7.9 | x < 3.5

x > 7.9 & x < 3.5
# ----

# Vectors ----

vec <- c(1, 2, 3, 4)
vec  # Print the contents

vec <- c(10.4, 5.6, 3.1, 6.4, 21.7)  # Numeric vector with 5 elements
length(vec)

typeof(vec)

c(vec, vec)

c(vec, "Mary")

vec[2]

vec[-2]

vec[c(1, 3)]

vec[1:3]

mask <- vec > 6
vec[mask]

vec <- c(10.4, 5.6, 3.1, 6.4, 21.7)

vec^2

log(vec)
exp(vec)
sin(vec)

vec_1 <- c(1, 3, 5, 7, 9)
vec_2 <- c(0, 2, 4, 6, 8)
vec_1 + vec_2

vec_1 <- c(1, 3, 5, 7, 9)
vec_2 <- c(0, 2)
vec_1 * vec_2

c(0, 2, 0, 2, 0)
# ----

# Lists ----

l <- list(1, FALSE, "hello")  # List without name 
l <- list(a = c(1, 2, 3), b = "hello", c = c(TRUE, FALSE))  # List with name

l_mix <- list(
    vec = c(1, 2, 4),  # Vector
    l = list(v = "Hello"),  # List
    mat = matrix(c(1, 0, 0, 1), nrow = 2)  # Matrix
)

length(l_mix)

dim(l_mix$mat)

names(l_mix)

names(l_mix$l)

l_mix[1]
class(l_mix[1])

l_mix[c('vec', 'l')]
class(l_mix[c('vec', 'l')])

l_mix[[1]]
class(l_mix[[1]])

l_mix[['vec']]
class(l_mix[['vec']])

l_mix$vec
class(l_mix$vec)

l_mix[[4]] <- c(TRUE, FALSE, NA)

l_mix$new <- "This is new!"
# ----

# Functions ----

first_func <- function() {
    # Empty body
    
    # No outputs
}
first_func()

first_func <- function() {
    print("Hello, world!")
}
first_func()
# ----

# Conditional Statement ----

x <- sample(1:100, 1)
if (x > 50) {
    print("Pass")
}

x <- sample(1:100, 1)
if (x > 50) {
    print("Pass")
} else {
    print("Fail")
}

x <- sample(1:100, 1)
if (x > 50) {
    print("Pass")
} else if (x > 30 & x <= 50) {
    print("Check")
} else {
    print("Fail")
}
# ----

# Loops ----

for (i in 1:10) {
    print(i)
}

steps <- c(2, 4, 6, 8, 10)
for (i in steps) {
    print(i)
}

char_vec <- c("a", "b", "c", "d")
for (i in char_vec) {
    print(i)
}

# Loop 2
for (i in 1:4) {
    print(char_vec[i])
}

# Loop 3
for (i in seq_along(char_vec)) {
    print(char_vec[i])
}

dat.list <- list(
    el_1 = c(1, 2, 3, 4, 5),
    el_2 = c(3.4, 2.3, 3.1, 4),
    el_3 = c(19.3, 12.5, 6.1)
)
# Set up an empty numeric vector to store the means. 
mean.vec <- vector(mode = "numeric", length = length(dat.list))

# Run the for loop 
for (i in seq_along(dat.list)) {
    mean.vec[i] <- mean(dat.list[[i]]) 
}
mean.vec

my.data.frame <- data.frame(var.1 = 1:6, var.2 = 8:13)
for (i in 1:nrow(my.data.frame)) {
    for (j in 1:ncol(my.data.frame)) {
        print(my.data.frame[i, j])
    }   
}

dat.list <- list(
    el_1 = c(1, 2, 3, 4, 5),
    el_2 = c(3.4, 2.3, 3.1, 4),
    el_3 = c(19.3, 12.5, 6.1)
)
lapply(dat.list, mean)
# ----
