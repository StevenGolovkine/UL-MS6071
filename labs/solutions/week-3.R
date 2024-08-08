################################################################################
# Possible solution for lab - ETL
################################################################################

# Load libraries
library(ggplot2)
library(ggimage)
library(httr2)
library(rvest)
library(tidyverse)


# ----
# PART 1 - The Movies Database
# ----

# 1. Connect to the database

URL <- 'moviesdatabase.p.rapidapi.com'
KEY <- 'c969b91d3bmsh6c179531cff8605p1360e5jsn834a6a15f4f5'

req <- request(glue::glue('https://', URL)) |>
    req_headers(
        'X-RapidAPI-Host' = URL,
        'X-RapidAPI-Key' = KEY
    )

# 2. Go on [IMDB](https://www.imdb.com), find a tv serie you like, and get
# it's id (it is in the URL).

# For The Simpsons, it is tt0096697.

# 3. Make a request to obtain all the identifiers for the episodes in the
# series. Transform it to a tibble.

resp <- req |>  
    req_url_path_append("titles") |>
    req_url_path_append("series") |> 
    req_url_path_append("tt0096697") |> 
    req_perform()

episodes <- resp |> 
    resp_body_json() |> 
    pluck("results") |>
    map_dfr(function(x) {x = flatten(x)}) |>
    as_tibble()

# Too many requests errors may happen. So, I'll focus on the first five seasons.
episodes <- episodes |> filter(seasonNumber < 6)

# 4. For each episode, make a request to obtain some information about it
# (name, duration, storyline, ...)

request_episode_info <- function(tconst, req) {
    resp <- req |>  
        req_url_path_append("titles") |>
        req_url_path_append(tconst) |> 
        req_perform()
    resp |>
        resp_body_json() |> 
        pluck("results") |> 
        list()
}

# Take some time to run...
episodes <- episodes |> 
    rowwise() |> 
    mutate(info = request_episode_info(tconst, req))


# 5. For each episode, make a request to obtain their ratings.

request_episode_ratings <- function(tconst, req) {
    resp <- req |> 
        req_url_path_append("titles") |>
        req_url_path_append(tconst) |> 
        req_url_path_append("ratings") |> 
        req_perform()
    resp |> 
        resp_body_json() |> 
        pluck("results") |> 
        list()
}

# Take some time to run...
episodes <- episodes |> 
    rowwise() |> 
    mutate(ratings = request_episode_ratings(tconst, req))


# 6. Clean the collected data to make a proper tibble.

episodes <- episodes |>
    # For the info column
    unnest_wider(col = info) |>  # Expand the nest list.
    select(-c("_id", "id", "releaseYear")) |>   # Remove unnecessary column.
    # For primaryImage
    unnest_wider(col = primaryImage) |> 
    select(-c("id", "width", "height", "caption", "__typename")) |> 
    # For titleType
    unnest_wider(col = titleType) |> 
    select(
        -c("text", "id", "isSeries", "isEpisode", "__typename", "titleText")
    ) |> 
    unnest_wider(col = originalTitleText) |> 
    select(-"__typename") |> 
    rename(title = text) |> 
    # For releaseDate
    unnest_wider(col = releaseDate) |> 
    select(-"__typename") |> 
    rowwise() |> 
    mutate(date = dmy(glue::glue(day, month, year, .sep = "/"))) |> 
    # For ratings
    unnest_wider(col = ratings, names_repair = "unique") |> 
    select(-"tconst...9") |> 
    rename(tconst = tconst...1)

# 7. Plot the evolution of the rankings through the season.

# Take some time to run...
ggplot(episodes) +
    geom_point(aes(x = episodeNumber, y = averageRating)) +
    geom_image(
        aes(x = episodeNumber, y = averageRating, image = url),
        size = .05
    ) +
    facet_wrap(
        vars(seasonNumber),
        labeller = labeller(
            seasonNumber = ~ glue::glue("Season", .x, .sep = " ")
        )
    ) +
    xlab("Episodes") +
    ylab("Average ratings") +
    ggtitle("Ratings of the Simpson episode per season") +
    theme_bw()
    
# 8. Export the data set in csv form.

write_csv(episodes, file = "./simpsons.csv")

# 9. Automate the script.

# Save all the previous command in an RScript named week-3.R.
# Then, you can run the following lines to create a job that run your script
# once a month.

# f   <- system.file('../labs/solutions/week-3.R')
# cmd <- cronR::cron_rscript(f)

# cron_add(
#     command = cmd,
#     frequency = 'monthly', 
#     id = 'api_data',
#     description = 'Get API data'
# )

# ----
# PART 2 - Wikipedia scrapping
# ----

# 10. Go on the Wikipedia page of
# [Ireland counties](https://en.wikipedia.org/wiki/Counties_of_Ireland). Create
# a scrapper to get a list of all the counties in Ireland using this page.

WIKI_URL <- "https://en.wikipedia.org/wiki/"
URL <- "Counties_of_Ireland"
pages <- read_html(glue::glue(WIKI_URL, URL))

counties <- pages |>
    html_elements(".wikitable") |>
    html_table() |> 
    first()
columns_dupli <- duplicated(colnames(counties))
counties <- counties[, !columns_dupli] |> 
    filter(County != "") |> 
    mutate(County = str_replace_all(County, "\\[d\\]", "")) |> 
    mutate(County = str_replace_all(County, "\\[b\\]", ""))

# 11. Look at the page of the different counties and create a function to create
# the URL of the county. Create a new column in the tibble with the URL.

create_url <- function(name) {
    glue::glue(WIKI_URL, "County_", name, .sep = "")
}

counties <- counties |> 
    rowwise() |> 
    mutate(url = create_url(County))

# 12. Scrap the information box of each of the county to retrieve some
# information (established, area, elevation, population, postcode, ...)

get_information <- function(url) {
    # Get the information
    labels <- read_html(url) |> 
        html_elements(".infobox .infobox-label") |>
        html_text()
    data <- read_html(url) |> 
        html_elements(".infobox .infobox-data") |>
        html_text()
    
    labels <- labels |>
        str_replace_all("\\(\\w*\\)", "") |> 
        str_replace_all("[:punct:]", "") |>
        str_trim()
    data <- data |> 
        str_replace_all("\\(.*\\)", "") |> 
        str_trim()
    names(data) <- labels
    data
}

counties <- counties |> 
    rowwise() |> 
    mutate(wikipage = list(get_information(url))) |> 
    unnest_wider(col = wikipage, names_repair = "unique")

# 13. Perform some cleaning and export the data as csv.

counties <- counties |> 
    # Rename some columns
    rename(
        `Native name` = `Native name(Irish)[41]`,
        `County town` = `County town...4`,
        `Most populous city` = `Mostpopulouscity/town`,
        `Province` = `Province...6`,
        `Region` = `Region...7`,
        `Rank area` = `Rank...15`
    ) |> 
    # Remove some columns
    select(-c(
        `Ulster-Scotsname(s)`, `Region...10`,
        `Province...11`, `County town...13`
    )) |> 
    # Fix typo in columns
    mutate(
        `County town` = str_replace_all(`County town`, "\\[\\w*\\]", ""),
        `Most populous city` = str_replace_all(`Most populous city`, "\\[\\w*\\]", ""),
        `Established` = str_replace_all(`Established`, "\\[\\w*\\]", "")
    )
    # etc...

write_csv(counties, './ireland_counties.csv')
