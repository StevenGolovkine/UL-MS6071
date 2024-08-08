################################################################################
# Possible solution for lab - Data visualisation
################################################################################

library(tidyverse)
library(ggthemes)

# ----
# Part 1 - Exploratory data analysis
# ----

# Variables
COLORS <- c(
    "Bug" = "#94A038",
    "Dark" = "#4E4240",
    "Dragon" = "#5360D9",
    "Electric" = "#F1C242",
    "Fairy" = "#DF77E9",
    "Fighting" = "#EF8733",
    "Fire" = "#D3333F",
    "Flying" = "#8DB7EA",
    "Ghost" = "#6A436D",
    "Grass" = "#5B9F3D",
    "Ground" = "#88542C",
    "Ice" = "#70D5FB",
    "Normal" = "#9FA19F",
    "Poison" = "#8746C4",
    "Psychic" = "#DD5079",
    "Rock" = "#AEA985",
    "Steel" = "#6F9FB5",
    "Water" = "#437EE7"
)

# 0. Load the data
df <- read_csv('./ressources/pokemon.csv')

# 1. Count the number of Pokemon for each type (use only type_1), and compute
# their proportions and percentages. Create a bar plot with these results.
# Comments on the distribution.
df_count <- df |>
    count(type_1) |> 
    mutate(
        proportion = n / sum(n),
        pct = 100 * proportion
    )

ggplot(df_count, aes(x = type_1, y = pct, fill = type_1)) +
    geom_bar(stat = 'identity') +
    scale_fill_manual(values = COLORS) +
    xlab("") +
    ylab("Percentage") +
    ylim(c(0, 15)) +
    theme_bw() +
    theme(
        legend.position = 'none'
    )

# 2. Do the same for each individual generation. Create a bar plot for each
# generation. Comments on the distribution, compare specifically with the
# distribution from the previous question. 
df_count <- df |>
    group_by(generation) |> 
    count(type_1) |> 
    mutate(
        proportion = n / sum(n),
        pct = 100 * proportion
    )

ggplot(df_count, aes(x = generation, y = pct, fill = type_1)) +
    geom_bar(stat = 'identity') +
    facet_wrap(~ type_1) +
    scale_x_continuous(breaks = 1:8) +
    scale_fill_manual(values = COLORS) +
    xlab("") +
    ylab("Percentage") +
    theme_bw() +
    theme(
        legend.position = 'none'
    )

# 3. Plot the distribution of heights. Comments on the distribution (mean, median
# skewness, ...).
ggplot(df, aes(x = height_m)) +
    geom_histogram(bins = 50) +
    xlab("Height") +
    ylab("Count") +
    theme_bw()

ggplot(df, aes(x = height_m)) +
    geom_boxplot() +
    xlab("Height") +
    theme_bw() +
    theme(
        axis.text.y = element_blank(), axis.ticks.y = element_blank()
    )

# There is one Pokemon with a very large height (100m). Let's plot the
# distribution without this Pokemon.
df |>
    filter(height_m < 100) |> 
    ggplot(aes(x = height_m)) +
        geom_histogram(bins = 50) +
        xlab("Height") +
        ylab("Count") +
        theme_bw()

df |>
    filter(height_m < 100) |> 
    ggplot(aes(x = height_m)) +
        geom_boxplot() +
        xlab("Height") +
        theme_bw() +
        theme(
            axis.text.y = element_blank(), axis.ticks.y = element_blank()
        )

# 4. Plot the distribution of attack. Try different number of bins in the
# histogram. 
ggplot(df, aes(x = attack)) +
    geom_histogram(bins = 20) +
    xlab("Attack stat") +
    ylab("Count") +
    theme_bw()

ggplot(df, aes(x = attack)) +
    geom_histogram(bins = 30) +
    xlab("Attack stat") +
    ylab("Count") +
    theme_bw()

# 5. Plot a boxplot of the attack per type and per generation. Comment your
# results.
ggplot(df, aes(x = attack, y = factor(generation), fill = type_1)) +
    geom_boxplot() +
    facet_wrap(~ type_1) +
    scale_fill_manual(values = COLORS) +
    xlab("Attack stat") +
    theme_bw() +
    theme(
        legend.position = 'none'
    )

# 6. Plot a scatter plot of the attack vs the defense. Create one per generation
# and one per type. 
ggplot(df, aes(x = attack, y = defense)) +
    geom_point() +
    xlab("Attack stat") +
    ylab("Defense stat") +
    theme_bw()

ggplot(df, aes(x = attack, y = defense)) +
    geom_point(data = transform(df, type_1 = NULL), color = "grey85") +
    geom_point(aes(color = type_1)) +
    facet_wrap(~ type_1) +
    scale_color_manual(values = COLORS) +
    xlab("Attack stat") +
    ylab("Defense stat") +
    theme_bw() +
    theme(
        legend.position = 'none'
    )

# 7. Create a small summary of your findings on this dataset.

# Blah, blah, blah

# ----
# Part 2 - Bad data visualisation
# ----

data_list <- readRDS('./ressources/data_week4.rds')


# Life expectancy
life_exp <- data_list$life_exp
life_exp |> 
    arrange(meanLifeExp) |> 
    mutate(continent = factor(continent, levels = continent)) |> 
    ggplot(aes(x = continent, y = meanLifeExp)) +
        geom_segment(aes(xend = continent, yend = 0)) +
        geom_point(size = 4, color = "red") +
        coord_flip() +
        xlab('') +
        ylab('Mean Life Expectancy') +
        ggtitle('Life Expectancy: 2007') +
        theme_tufte() +
        theme(
            axis.ticks.y = element_blank()
        )

# Covid cases
covid_case <- data_list$covid_case
covid_case |> 
    ggplot(aes(x = date, y = cases)) +
        geom_point(size = 3) +
        geom_line() +
        geom_label(aes(label = cases)) +
        scale_x_date(date_breaks = "1 day") +
        labs(
            x = "",
            y = "",
            title = "New cases per day",
            subtitle = paste("Total cases: ", sum(covid_case$cases))
        ) +
        theme_tufte() +
        theme(
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank()
        )

# Disability causes
disablity_cause <- data_list$disablity_cause
disablity_cause <- disablity_cause |> mutate(Disability_2 = Disability)
disablity_cause |> 
    ggplot(aes(x = Year, y = Percentage)) +
        geom_line(
            aes(group = factor(Disability)),
            data = transform(disablity_cause, Disability_2 = NULL),
            color = "grey85"
        ) +    
        geom_line(linewidth = 2) +
        geom_label(
            aes(y = Percentage + 2, label = Percentage),
            data = filter(disablity_cause, (Year == 1975) | (Year == 2010))
        ) +
        facet_wrap(~ Disability_2) +
        scale_x_continuous(breaks = seq(1975, 2010, 5)) +
        labs(
            x = "",
            y = "",
            title = "Major Cause of Disability",
            subtitle = "(percentage)"
        ) +
        theme_tufte() +
        theme(
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank()
        )

# Percentage of senior managers
managers <- data_list$managers
managers |> 
    mutate(Country = fct_reorder(Country, Women)) |> 
    ggplot() +
        geom_segment(
            aes(x = Women, xend = Men, y = Country, yend = Country),
            color = "grey85"
        ) +
        geom_point(
            aes(x = value, y = Country, color = name),
            pivot_longer(managers, cols = c("Men", "Women")),
            size = 2
        ) +
        geom_point(
            aes(x = value, y = Country, color = name), shape = 2, size = 3,
            data = filter(
                pivot_longer(managers, cols = c("Men", "Women")),
                Country == 'OECD average'
            )
        ) +
        geom_label(
            aes(x = Women - 1, y = Country, label = Country),
            size = 3
        ) +
        labs(
            x = "Percentage",
            y = "",
            title = "Percentage of Employed Who Are Senior Managers, by Gender",
            subtitle = "(percentage)",
            color = ""
        ) +
        theme_tufte() +
        theme(
            legend.position = 'bottom',
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
        )
