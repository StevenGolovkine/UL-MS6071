################################################################################
# Script file for R Basics
################################################################################


# This is a comment.
# And, here another one!

x <- 1  # Nothing is printed...

10 + 4 / 2

(10 + 4) / 2

x <- 1





x <- c(1, 3.5, 7.9, 10, 12.3, NA, 15.8)

x == 3.5

x != 3.5

x > 3.5

!(x > 3.5)

x <= 3.5

x > 7.9 | x < 3.5

x < 7.9 & x > 3.5


A <- c(1, 2, 3)
mode(A)

A <- 1:10
print(A)

length(A) <- 3
print(A)

length(A) <- 6
print(A)

null_vector <- integer()  # Vector of integer of length zero.

mode(null_vector)

length(null_vector)

A <- c(1, 2, 3)

typeof(A)  # type of A
class(A)  # class of A
print(A)  # print A

class(A) <- "my_class"

print.my_class <- function(obj) {
    print(paste("Vector: ", obj))
}

typeof(A)  # type of A
class(A)  # class of A
print(A)  # print A


vec <- c(10.4, 5.6, 3.1, 6.4, 21.7)  # Numeric vector with 5 elements
vec  # Print the contents

mode(vec)
length(vec)

c(vec, 0, vec)

c(vec, "Mary")

vec[2]

vec[-2]

vec[c(1, 3)]

vec[1:3]

mask <- vec > 6
vec[mask]

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


lst <- list(1, FALSE, "hello")  # List without name 
lst <- list(a = c(1, 2, 3), b = "hello", c = c(TRUE, FALSE))  # List with name

lst <- list(
    vec = c(1, 2, 4),  # Vector
    lst = list(v = "Hello"),  # List
    mat = matrix(c(1, 0, 0, 1), nrow = 2)  # Matrix
)

mode(lst)
length(lst)

names(lst)

lst[c(1, 2)]

lst[c('vec', 'lst')]

lst[[1]]

lst$vec

lst[[4]] <- c(TRUE, FALSE, NA)

lst$new <- "This is new!"


function_name <- function(argument_1, argument_2, ...) {
    
    # Body/code
    
    # Output
    
}

function_name(1, 'A')
function_name(argument_1 = 1, argument_2 = 'A')
function_name(1, argument_2 = 'A')

function_name <- function(argument_1, arg_with_default=1, ...) {
    
    # Body/code
    
    # Output
    
}

first_func <- function() {
    # Empty body
    
    # No outputs
}

first_func()

first_func <- function() {
    print("Hello, world!")
}

first_func()

early_exit <- function(x) {
    if (x < 0) {
        return("Negative input not allowed!")
    }
    # rest of the function
}

multiple_returns <- function(x) {
    if (x < 0) {
        return("Negative input not allowed!")
    } else if (x == 0) {
        return("Input is zero!")
    }
    # rest of the function
}

inside_loops <- function(numbers) {
    for (num in numbers) {
        if (num < 0) {
            return("Negative number found")
        }
    }
    # rest of the function
}


Rscript script.R


if (condition) {
    # Execute this code
}
    
# Otherwise continue with rest of code

x <- sample(1:100, 1)
if (x > 50) {
    print("Pass")
}


if (condition) {
    # Execute this code
} else {
    # Execute this code instead
}

x <- sample(1:100, 1)
if (x > 50) {
    print("Pass")
} else {
    print("Fail")
}


if (condition_1) {
    # Execute code chunk 1
} else if (condition_2) {
    # Execute code chunk 2
} else {
    # Execute this code
}

x <- sample(1:100, 1)
if (x > 50) {
    print("Pass")
} else if (x > 30 && x <= 50) {
    print("Check")
} else {
    print("Fail")
}


for (iterator_variable in some_sequence) {
    # do something
}

for (i in 1:10) {
    print(i)
}

char_vec <- c("a", "b", "c", "d")

# Loop 1
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

data_list <- list(
    el_1 = c(1, 2, 3, 4, 5),
    el_2 = c(3.4, 2.3, 3.1, 4),
    el_3 = c(19.3, 12.5, 6.1)
)

# Set up an empty numeric vector to store the means. 
mean_vec <- vector(
    mode = "numeric",
    length = length(data_list)
)

# Run the for loop 
for (i in seq_along(data_list)) {
    mean_vec[i] <- mean(data_list[[i]]) 
}

mean_vec

my_data_frame <- data.frame(var1 = 1:6, var2 = 8:13)
for (i in 1:nrow(my_data_frame)) {
    for (j in 1:ncol(my_data_frame)) {
        print(my_data_frame[i, j])
    }   
}

data_list <- list(
    el_1 = c(1, 2, 3, 4, 5),
    el_2 = c(3.4, 2.3, 3.1, 4),
    el_3 = c(19.3, 12.5, 6.1)
)
sapply(data_list, mean)


# Creating a data frame
student_data <- data.frame(
    student_id = c(1, 2, 3, 4),
    name = c("Alice", "Bob", "Cathy", "David"),
    age = c(22, 23, 21, 24),
    grade = c("A", "B", "B", "C")
)

student_data

student_data$name

student_data$class <- c("Maths", "Stat", "Stat", "Maths")
student_data

new_student <- data.frame(
    student_id = 5,
    name = "Edward",
    age = 23,
    grade = "A",
    class = "Stat"
)
student_data <- rbind(student_data, new_student)
student_data


install.packages("package_name")

library(package_name)

library(package_name)

update.packages("package_name")

library(dplyr)

library(MASS)

dplyr::select(...)  # For the dplyr function
MASS::select(...)   # For the MASS function


