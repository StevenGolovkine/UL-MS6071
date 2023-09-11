################################################################################
# Script file for Week 03 - Extract, Transform and Load
################################################################################

library(tidyverse)

THEONE_TOKEN <- Sys.getenv("THEONE_TOKEN")

# Load the library
library(httr2)

req <- request("https://the-one-api.dev/v2") |>  # Create the request
    req_auth_bearer_token(THEONE_TOKEN)  # API token

resp <- req |>  
    req_url_path_append("character") |>  # Create the route
    req_perform()  # The request is only sent here

resp$status_code

characters <- resp |> 
    resp_body_json() |>  # Read the results
    pluck("docs") |>  # Extract the docs field (specific to this API)
    map_dfr(function(x) {x = flatten(x)}) |>  # Convert to tibble
    as_tibble()

characters |> filter(name == 'Gollum')

id_gollum <- characters |> filter(name == 'Gollum') |> pull(`_id`)
quotes <- req |>
    req_url_path_append(paste0("character/", id_gollum, "/quote")) |> 
    req_perform() |> 
    resp_body_json() |> 
    pluck("docs")

quotes[[10]]$dialog

# Load the library
library(rvest)

WIKI_URL <- "https://en.wikipedia.org/wiki/"
URL <- "List_of_Billboard_Hot_100_number_ones_of_2022"
pages <- read_html(paste0(WIKI_URL, URL))

tables <- pages |>
    html_elements("table") |>  # Get HTML elements with table attributes
    html_table()  # Convert the HTML tables into tibbles


# Load the library
head(tables[[2]])


library(httr2)
library(tidyverse)

THEONE_TOKEN <- Sys.getenv("THEONE_TOKEN")

req <- request("https://the-one-api.dev/v2") |>  # Create the request
    req_auth_bearer_token(THEONE_TOKEN)  # API token

resp <- req |>  
    req_url_path_append("character") |>  # Create the route
    req_perform()  # The request is only sent here
characters <- resp |> 
    resp_body_json() |>  # Read the results
    pluck("docs") |>  # Extract the docs field (specific to this API)
    map_dfr(function(x) {x = flatten(x)}) |>  # Convert to tibble
    as_tibble()

resp <- req |>  
    req_url_path_append("movie") |>  # Create the route
    req_perform()  # The request is only sent here
movie <- resp |> 
    resp_body_json() |>  # Read the results
    pluck("docs") |>  # Extract the docs field (specific to this API)
    map_dfr(function(x) {x = flatten(x)}) |>  # Convert to tibble
    as_tibble()

resp <- req |>  
    req_url_path_append("quote") |>  # Create the route
    req_perform()  # The request is only sent here
quotes <- resp |> 
    resp_body_json() |>  # Read the results
    pluck("docs") |>  # Extract the docs field (specific to this API)
    map_dfr(function(x) {x = flatten(x)}) |>  # Convert to tibble
    as_tibble()


head(movie)

head(characters)

head(quotes)

dataset_join <- quotes |>
    left_join(movie, by = join_by("movie" == "_id")) |> 
    full_join(characters, by = join_by("character" == "_id"))
head(dataset_join)


write_csv(dataset_join, "./path/lord_of_the_rings.csv")

write_rds(dataset_join, "./path/lord_of_the_rings.rds")


library(cronR)

f <- system.file("r_script.R")  # get the path of the file
cmd <- cron_rscript(f, rscript_args = c("args1", "args2"))

cron_add(cmd, frequency = 'hourly', id = 'job1')

cron_add(cmd, frequency = 'daily', at = '08:20', days_of_week = 0)

cron_add(
    cmd, frequency = 'monthly', at = '14:00',
    days_of_month = 'first', days_of_week = '*'
)

cron_ls()

cron_clear(ask = FALSE)


library(logger)

log_threshold(TRACE)

log_info('Starting the script...')
log_debug('Debugging line')
log_error("No! 1 + 1 is not equal to 3.")

file <- './source/temp.log'
log_appender(appender_file(file))
log_info('Written in the temp.log file.')

readLines(file)


