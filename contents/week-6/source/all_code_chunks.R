################################################################################
# Script file for Interactivity
################################################################################


install.packages('plotly')

library(tidyverse)
library(plotly)

df <- read_csv('./source/states.csv')
head(df)

plot_ly(data = df, x = ~n_representatives)


plot_ly(
    data = df,
    x = ~factor(n_representatives)  # Globally assign x
) |> 
    add_histogram() |>  # Add a histogram layer
    group_by(n_representatives) |> 
    summarise(n = n()) |>  # Modify the data in the plotly object
    add_text(  # Add a layer of text, x is the same as before
        text = ~n, y = ~n, 
        textposition = "top middle", 
        cliponaxis = FALSE
    )

plot_ly(
    data = df,
    type = 'scatter',
    mode = 'markers',
    x = ~population_2020,
    y = ~total_area_mi2,
    text = ~paste(
        'Population:', population_2020,
        '\nTotal area in mi^2:', total_area_mi2,
        '\nNumber of representative:', n_representatives
    ),
    marker = list(
        color = ~n_representatives, 
        colorbar = list(
            title = "Number of representatives",
            orientation = 'h'
        ),
        colorscale = 'Viridis',
        showscale = TRUE
    )
) |> 
    layout(
        title = 'Population vs Total Area of American States (in 2020)',
        xaxis = list(title = 'Population', color = 'red'),
        yaxis = list(title = "Total area in miles squared")
    )

gg <- ggplot(df) +
    geom_point(
        aes(x = population_2020, y = total_area_mi2, text = state)
    ) +
    xlab('Population in 2020') +
    ylab('Total area in square miles') +
    theme_bw()
gg

ggplotly(gg, tooltip = 'text')


selectInput(
    inputId = "n_bins", label = "Number of bins:",
    choices = c(10, 20, 35, 50), selected = 20
)

sliderInput(
    inputId = "n_bins", label = "Number of bins:", 
    min = 1, max = 50, value = 30
)

sliderInput(
    inputId = "n_bins", label = "Number of bins:", 
    min = 1, max = 50, value = 30, step = 5
)
plotOutput("distPlot")

output$distPlot <- renderPlot({
   x <- faithful[, 2]  # Old Faithful Geyser data
   bins <- seq(min(x), max(x), length.out = input$n_bins + 1)
   hist(x, breaks = bins, col = 'darkgray', border = 'white',
        xlab = 'Waiting time to next eruption (in mins)',
        main = 'Histogram of waiting times')
})


install.packages('flexdashboard')
install.packages('DT')

library(flexdashboard)

valueBox(value = "10", icon = "fa-github")

pr <- 25
valueBox(
    value = pr, icon = "fa-comments",
    color = ifelse(pr > 10, "warning", "primary")
)

library(flexdashboard)

valueBox(value = "10", icon = "fa-github")

pr <- 30
gauge(
    value = pr, min = 0, max = 50,
    sectors = gaugeSectors(
        success = c(0, 10), warning = c(11, 25), danger = c(26, 50)
    )
)


