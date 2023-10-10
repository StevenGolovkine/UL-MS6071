################################################################################
# Script file for Week 07 - Memory Issues
################################################################################


x <- 3L  # Create the value 3 of type integer.

as.integer(2^31 - 1)  
.Machine$integer.max

as.integer(2^31)  
is.double(2^31)

46360 * 46360  # 46360 is stored as a double.
46360L * 46360L  # 46360L is stored as an integer.

sum(1:304)  # sum(1:304) is stored as an integer.
sum(1:304) * sum(1:304)  # is a product of integer.
46360^2  # 46360 is stored as a double
sum(1:304)^2  # ...

object.size(3L)  # on a 64 bit processor

.Machine$sizeof.pointer


source('./source/functions.R')

(1.0 - 0.5) == 0.5

(1.0 - 0.9) == 0.1

x <- 100 * (1 - 0.34)

paste('Using as.integer(x):', as.integer(x))
paste('Using floor(x):', floor(x))
paste('Using round(x):', round(x))
paste('Computing x - 66:', x - 66)

bin2dec(1010.101)

dec2bin(10.625, 3)

dec2bin(0.2, 30)

A <- 2^150
B <- A + 2^98
A == B

A <- 2^150
B <- A + 2^97
A == B


library(tidyverse)

# Using bash

# head source/airports.csv

# tail -n 2 source/airports.csv

# sort -t, -k3 source/airports.csv | head -n 5

# awk -F, '$4 = "CA"' source/airports.csv | head -n 5

# cut -f6,7 -d, source/airports.csv | head -n 5

# cut -f4 -d, source/airports.csv | sort | uniq -c | head -n 5

# cut -f4 -d, source/airports.csv | grep -v NA | head -n 5

# cut -f4 -d, source/airports.csv| tail -n +2 | sort | uniq | wc -l

# sqlite3 -csv airports.sqlite3 '.import source/airports.csv airports'

connection <- DBI::dbConnect(
    RSQLite::SQLite(), 
    "source/airports.sqlite3"
)
airports_df <- dplyr::tbl(connection, "airports")

airports_df |> 
    count(state) |> 
    show_query()

airports_df |> 
    count(state) |> 
    collect()


