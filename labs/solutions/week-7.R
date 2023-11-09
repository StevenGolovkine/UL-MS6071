################################################################################
# Possible solution for week 7 lab - Memory Issues
################################################################################

################################################################################
# Part 2 - Some statistics with bash

# 5. Count the number of lines.

# wc -l airline.csv

# 6. Show the first five flights from Des Moines (DSM) to Chicago O'Hare (ORD).

# awk -F, '$17 == "DSM" && $18 == "ORD"' airline.csv | head -n5

# 7. Count the number of flights for each flight number in 1993 and save it to
# a new file.

# awk -F, '$1 == "1993"' airline.csv | cut -f10 -d, | sort | uniq -c > test.csv

# 8. Does the column 29 (LateAircraftDelay) contains only `NA` values?

# cut -f29 -d, airlines.csv | grep -v NA

################################################################################
# Part 3 - Analysing the data within R

# 9. Load the library and connect to the database.
library(tidyverse)
library(DBI)


connection <- DBI::dbConnect(
    RSQLite::SQLite(), 
    "airline.sqlite3"
)

# 10. Create a "lazy" tibble.
flights_df <- dplyr::tbl(connection, "airline")

# 11. Count the number of rows.
flights_df |> tally()

# 12. Get the name of the columns.
names(flights_df)

# 13. Created a new column `delayed` that is `TRUE` if the flight has been
# delayed and `FALSE` otherwise. Show the SQL query.
flights_df |>
    mutate(is_delayed = ArrDelay > 0) |> 
    show_query()
flights_df <- flights_df |>
    mutate(is_delayed = ArrDelay > 0)

# 14. Get all the flights from JFK to SFO. Bring the data to R. 
jfk_to_sfo <- flights_df |>
    filter(Origin == 'JFK', Dest == 'SFO') |> 
    collect()

# 15. When is the best hour of the day to fly to minimise delays on departure? What if we want to do it by year?

delays <- flights_df |> 
    mutate(DepHour = floor(CRSDepTime / 100)) |> 
    group_by(DepHour) |> 
    summarise(
        MeanDepDelay = mean(DepDelay, na.rm = TRUE),
        SdDepDelay = sd(DepDelay, na.rm = TRUE)) |> 
    collect()

delays_per_year <- flights_df |> 
    mutate(DepHour = floor(CRSDepTime / 100)) |> 
    group_by(Year, DepHour) |> 
    summarise(
        MeanDepDelay = mean(DepDelay, na.rm = TRUE),
        SdDepDelay = sd(DepDelay, na.rm = TRUE)
    ) |> 
    collect()

# 16. Create a plot with the results.
delays_per_year |> 
    ggplot(aes(
        x = DepHour, y = MeanDepDelay,
        ymin = MeanDepDelay - SdDepDelay,
        ymax = MeanDepDelay + SdDepDelay)
    ) +
    geom_pointrange() +
    facet_wrap(~ Year) +
    ylim(c(-50, 100)) +
    ylab("Mean departure delay") +
    xlab("Time of Day") +
    theme_bw()
