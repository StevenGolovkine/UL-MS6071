################################################################################
# Script file for Week 04 - Data Visualisation
################################################################################


library(tidyverse)

df <- tribble(
    ~Vaccinated, ~Survive, ~Count,
    "Yes", "Yes", 24,
    "Yes", "No", 0,
    "No", "Yes", 0,
    "No", "No", 24
)

ggplot(df, aes(x = Vaccinated, y = Count)) +
    geom_col(aes(fill = Survive), position = 'dodge') + 
    scale_y_continuous(name = "Count") +
    theme_bw()

df <- tribble(
    ~Component, ~Failure, ~Count,
    "A", "Failure", 12,
    "A", "No Failure", 6,
    "B", "Failure", 8,
    "B", "No Failure", 14,
)

ggplot(df, aes(x = Component, y = Count)) +
    geom_col(aes(fill = Failure), position = 'dodge') + 
    scale_y_continuous(name = 'Count') +
    theme_bw()



library(tidyverse)
library(ggthemes)

df <- tibble(
    group = factor(rep(1:4, each = 11)),
    x = c(anscombe$x1, anscombe$x2, anscombe$x3, anscombe$x4),
    y = c(anscombe$y1, anscombe$y2, anscombe$y3, anscombe$y4)
)

ggplot(df, aes(x = x, y = y)) +
    geom_point() +
    geom_smooth(method = 'lm') + 
    facet_wrap(vars(group), labeller = labeller(
        group = ~ paste("Dataset", .x)
    )) +
    theme_bw()


A <- list(
    list(
        group = "g1",
        response = rnorm(100, 1.2, 1)
    ),
    list(
        group = "g2",
        response = rnorm(100, -.5, 1.2)
    ),
    list(
        group = "g3",
        response = rnorm(100, 0.5, 1)
    ),
    list(
        group = "g4",
        response = rt(100, 5, 1)
    ),
    list(
        group = "g5",
        response = rexp(100, 0.5)
    )
)

df <- tibble(A) |>
    unnest_wider(col = A) |> 
    unnest(col = response)
df_mean <- df |> 
    group_by(group) |> 
    summarise(mean_ = mean(response)) |> 
    mutate(x_pos = c(0.75, 1.75, 2.75, 3.75, 4.75))

boxplot(df$response ~ df$group)

ggplot(df, aes(x = group, y = response)) + 
    geom_tufteboxplot() +
    geom_text(
        data = df_mean, 
        mapping = aes(x = x_pos, y = mean_, label = round(mean_, 2)),
        color = "darkred", na.rm = TRUE
    ) +
    scale_x_discrete(
        labels=c("g1" = "1", "g2" = "2", "g3" = "3", "g4" = "4", "g5" = "5")
    ) + 
    xlab('Group') +
    ylab('Response') +
    theme_tufte() +
    theme(
        axis.ticks.x = element_blank()
    )


library(tidyverse)

df <- read_csv('./source/StudentsPerformance.csv')
head(df)

df_count <- df |>
    count(`parental level of education`) |> 
    mutate(
        proportion = n / sum(n),
        pct = 100 * proportion
    )

ggplot(df, aes(x = `parental level of education`)) +
    geom_bar() +  # Barchart
    theme_bw()


ggplot(df_count, aes(x = `parental level of education`, y = pct)) + #
    geom_bar(stat = 'identity') +  # Barchart
    ylim(c(0, 100)) +
    theme_bw()

ggplot(df, aes(x = `math score`)) +
    geom_histogram() +  # Histogram
    theme_bw()


ggplot(df, aes(x = `math score`)) +
    geom_histogram(bins = 15) +  # Histogram
    theme_bw()


temp <- tibble(x = rnorm(1000))
density_estim <- density(temp$x, n = 1000, from = -4, to = 4)
temp$t <- density_estim$x
temp$density <- density_estim$y
ggplot(temp) +
    geom_histogram(
        aes(x = x, after_stat(density)),
        fill = 'white', color = 'black'
    ) +
    geom_line(aes(x = t, y = density), col = 'red', size = 2) +
    theme_void()

temp <- tibble(x = rbeta(1000, 2, 6))
density_estim <- density(temp$x, n = 1000, from = 0, to = 1)
temp$t <- density_estim$x
temp$density <- density_estim$y
ggplot(temp) +
    geom_histogram(
        aes(x = x, after_stat(density)),
        fill = 'white', color = 'black'
    ) +
    geom_line(aes(x = t, y = density), col = 'red', size = 2) +
    theme_void()


temp <- tibble(x = rbeta(1000, 6, 2))
density_estim <- density(temp$x, n = 1000, from = 0, to = 1)
temp$t <- density_estim$x
temp$density <- density_estim$y
ggplot(temp) +
    geom_histogram(
        aes(x = x, after_stat(density)),
        fill = 'white', color = 'black'
    ) +
    geom_line(aes(x = t, y = density), col = 'red', size = 2) +
    theme_void()


temp <- tibble(x = c(rnorm(500, 2), rnorm(500, -2)))
density_estim <- density(temp$x, n = 1000, from = -5, to = 5)
temp$t <- density_estim$x
temp$density <- density_estim$y
ggplot(temp) +
    geom_histogram(
        aes(x = x, after_stat(density)),
        fill = 'white', color = 'black'
    ) +
    geom_line(aes(x = t, y = density), col = 'red', size = 2) +
    theme_void()

ggplot(df, aes(x = `math score`)) +
    geom_boxplot() +  # Boxplot
    theme_bw()


ggplot(df, aes(x = `math score`, y = `parental level of education`)) +
    geom_boxplot() +  # Boxplot
    theme_bw()




