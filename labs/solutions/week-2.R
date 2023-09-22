################################################################################
# Possible solution for week 2 lab - Tidyverse
################################################################################

# Load libraries
library(tidyverse)

# Variables
PATH_FILE <- './ressources/erasmus.csv'


# ----
# PART 1 - Data importation and cleaning
# ----

# 1. Load the data set.
df <- read_csv(PATH_FILE)


# 2. Remove the columns `project_reference`, `activity_mob`,
# `field_of_education`, `education_level`, `participant_profile,
# `sending_organisation_erasmus_code`, `group_leader` and
# `receiving_organisation_erasmus_code`.
df <- df |>
    select(-c(
        project_reference,
        activity_mob,
        field_of_education,
        education_level, participant_profile,
        sending_organisation_erasmus_code,
        receiving_organisation_erasmus_code,
        group_leader
    ))


# 3. Convert the columns `mobility_start_month` and `mobility_end_month` to
# Date.
df <- df |>
    mutate(
        mobility_start_month = ym(mobility_start_month),
        mobility_end_month = ym(mobility_end_month)
    )


# 4. Convert the columns `academic_year` to ordered factor and
# `participant_nationality`, `participant_gender`, `sending_country_code` and
# `receiving_country_code` to factor.
df <- df |> 
    mutate(
        academic_year = factor(academic_year, ordered = TRUE),
        participant_nationality = factor(participant_nationality),
        participant_gender = factor(participant_gender),
        sending_country_code = factor(sending_country_code),
        receiving_country_code = factor(receiving_country_code)
    )


# 5. Convert the columns `special_needs` and`fewer_opportunities` to boolean.
df <- df |> 
    mutate(
        fewer_opportunities = ifelse(fewer_opportunities == "Yes", 1, 0),
        special_needs = ifelse(special_needs == "Yes", 1, 0),
    ) |> 
    mutate(
        fewer_opportunities = as.logical(fewer_opportunities),
        special_needs = as.logical(special_needs)
    )


# 6. Is there any rows with inconsistencies, e.g., negative ages? Remove these
# rows.
df <- df |>
    filter(participant_age > 1 & participant_age < 100) |> 
    filter(participant_nationality != "-")


# ----
# PART 2 - Data manipulation
# ----

# 7. What is the mean mobility duration?
df |> 
    select(mobility_duration) |> 
    summarise(mean_duration = mean(mobility_duration))


# 8. How many people stayed more than 3 months in the UK in the academic year
# 2015-2016?
df |> 
    filter(
        academic_year == '2015-2016',
        receiving_country_code == 'UK',
        mobility_duration > 3
    ) |> 
    summarise(n_participants = sum(participants))


# 9. What are the top 5 countries that receive the most people?
df |> 
    select(c(receiving_country_code, participants)) |> 
    group_by(receiving_country_code) |> 
    summarise(n_participants = sum(participants)) |> 
    arrange(desc(n_participants)) |> 
    slice(1:5)


# 10. How many women went to Ireland during the academic year 2018=2019?
df |> 
    filter(
        participant_gender == 'Female',
        receiving_country_code == 'IE',
        academic_year == '2018-2019'
    ) |> 
    summarise(n_participants = sum(participants))


# 11. Within this subset of the data, what is the proportion of participants
# that go to Dublin?
df |> 
    filter(
        participant_gender == 'Female',
        receiving_country_code == 'IE',
        academic_year == '2018-2019'
    ) |> 
    mutate(in_Dublin = if_else(receiving_city == 'Dublin', TRUE, FALSE)) |> 
    group_by(in_Dublin) |> 
    summarise(n_participants = sum(participants)) |> 
    mutate(proportion = n_participants / sum(n_participants))


# 12. What is the correlation between the duration of the mobility and the age
# of the participant by participant nationality. Make sure to have enough data
# points estimate the correlation (let say more than 100).
df |> 
    select(participant_nationality, mobility_duration, participant_age) |> 
    group_by(participant_nationality) |> 
    nest() |> 
    mutate(n = map_dbl(data, ~ nrow(.x))) |> 
    filter(n > 100) |> 
    mutate(
        cor_dur_age = map_dbl(
            data, ~ cor(.x$mobility_duration, .x$participant_age)
        )
    )


# ----
# PART 3 - Plots
# ----

# For all the questions regarding the plots, make sure to set proper axis,
# titles, themes and other visual elements.

# 13. Plot the distribution of participant ages (regardless the number of
# participants).
ggplot(df) +
    geom_bar(aes(x = participant_age)) +
    labs(
        x = "Participant age",
        y = "Count",
        title = "Distribution of the age of participants"
    ) +
    theme_bw()


# 14. Plot the distribution of the ages by nationality and by gender. Create a
# grid for the different nationalities and have a different color for the
# gender. Select some nationalities to simplify the plot.
df |> 
    filter(
        participant_nationality == 'DE' |
        participant_nationality == 'PL' |
        participant_nationality == 'UK' |
        participant_nationality == 'ES' |
        participant_nationality == 'FR'
    ) |> 
    group_by(participant_nationality, participant_gender, participant_age) |> 
    summarise(n_participants = sum(participants)) |> 
    ggplot() +
        geom_histogram(aes(
            x = participant_age,
            y = after_stat(density),
            group = participant_gender,
            fill = participant_gender), binwidth = 20) +
        facet_grid(~ participant_nationality) +
        labs(
            x = "Participant age",
            y = "Density",
            title = "Distribution of the age of participants by gender for each nationality",
            fill = 'Gender'
        ) +
        theme_bw() +
        theme(
            legend.position = 'bottom'
        )


# 15. Plot the evolution of the number of participants through academic year for
# the top 5 countries that received the most participants.
df |> 
    filter(
        receiving_country_code == 'DE' |
        receiving_country_code == 'PL' |
        receiving_country_code == 'UK' |
        receiving_country_code == 'ES' |
        receiving_country_code == 'FR'
    ) |> 
    group_by(academic_year, receiving_country_code) |> 
    summarise(n_participants = sum(participants)) |> 
    ungroup() |> 
    ggplot() +
        geom_line(aes(
            x = academic_year,
            y = n_participants,
            group = receiving_country_code,
            color = receiving_country_code
        )) +
        labs(
            x = "Academic year",
            y = "Number of partcipants",
            color = 'Country'
        ) +
        theme_classic() +
        theme(
            legend.position = 'bottom'
        )


# 16. Plot the alluvial plot between the sending and receiving country by
# participant gender. To ease the visualisation, remove the rows where the
# receiving country is the sending country and only consider links with more
# than 50 participants. (You may need to install another package.)

# https://r-charts.com/flow/ggalluvial/#google_vignette
library(ggalluvial)
df |> 
    select(
        receiving_country_code,
        sending_country_code,
        participant_gender,
        participants
    ) |> 
    group_by(
        receiving_country_code, sending_country_code, participant_gender
    ) |> 
    summarise(n_participants = sum(participants)) |> 
    ungroup() |> 
    mutate(
        receiving_is_sending = if_else(
            as.character(receiving_country_code) ==
                as.character(sending_country_code),
            TRUE,
            FALSE
        )
    ) |> 
    filter(!receiving_is_sending, n_participants > 50) |> 
    ggplot(
        aes(
            axis1 = sending_country_code,
            axis2 = receiving_country_code,
            y = n_participants
        )
    ) +
    geom_alluvium(aes(fill = participant_gender)) +
    geom_stratum() +
    geom_text(
        stat = "stratum",
        aes(label = after_stat(stratum))
    ) +
    scale_x_discrete(expand = c(0.15, 0.05)) +
    labs(
        fill = 'Gender'
    ) +
    theme_void() +
    theme(
        legend.position = 'bottom'
    )

# 17. Propose your own plot.
