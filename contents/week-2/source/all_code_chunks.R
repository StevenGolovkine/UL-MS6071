################################################################################
# Script file for Tidyverse
################################################################################

library(tidyverse)

table1

table2

table3

vignette("tibble")

my_data_frame <- data.frame(
    year = c(1980, 1988, 1996, 1998, 2000, 2002, 1985, 2005),
    gender = c("M", "M", "F", "F", "M", "F", "M", "M"),
    wgt = c(71.5, 72.1, 73.7, 74.3, 75.2, 74.7, 89.6, 95.2)
)

my_tibble <- tibble(
    year = c(1980, 1988, 1996, 1998, 2000, 2002, 1985, 2005),
    gender = c("M", "M", "F", "F", "M", "F", "M", "M"),
    wgt = c(71.5, 72.1, 73.7, 74.3, 75.2, 74.7, 89.6, 95.2)
)

head(my_data_frame)
head(my_tibble)

str(my_data_frame)
str(my_tibble)

mode(my_data_frame)
mode(my_tibble)

length(my_data_frame)
length(my_tibble)

my_data_frame$year
my_tibble$year

my_data_frame[,"year"]  # returns a vector
my_tibble[,"year"]  # returns a tibble

my_data_frame[["year"]]
my_tibble[["year"]]


library(tidyverse)

df <- read_csv(
    file = 'filepath/filename.csv',
    col_names = TRUE
)

df <- read_csv(
    './ressource/wine_quality.csv',
    col_types = 'idddddddddddi'
)
head(df)

str(df)

summary(df)


library(tidyverse)

df <- tribble(
  ~student,   ~maths, ~physics,
  'Alice',        18,       15,
  'Bob',          14,       10,
  'Charlie',       8,       16
)

df |> pivot_longer(
  cols = c(maths, physics),
  names_to = 'course',
  values_to = 'grade'
)

df <- tribble(
  ~student,   ~course, ~grade,
  'Alice',    "maths",     18,
  'Alice',  "physics",     15,
  'Bob',      "maths",     14,
  'Bob',    "physics",     10,
  'Charlie',  "maths",      8,
  'Charlie',"physics",     16
)

df |> pivot_wider(
  names_from = 'course',
  values_from = 'grade'
)

df <- tribble(
  ~student,      ~grades,
  'Alice',    '18/15/20',
  'Bob',      '14/10/12',
  'Charlie',   '8/16/14'
)

df |> separate_wider_delim(
  cols = grades,
  delim = '/',
  names = c('maths', 'physics', 'french')
)


library(tidyverse)

df <- read_csv(
    './ressource/wine_quality.csv',
    col_types = 'idddddddddddi'
)

filter(data, condition_1, condition_2, condition_3)

data |> filter(condition_1, condition_2, condition_3)

filter(df, alcohol > 10)

df |> filter(alcohol > 10)

df |> filter(alcohol > 10, quality == 8)

arrange(data, column_1, column_2, column_3)

data |> arrange(column_1, column_2, column_3)

df |> arrange(fixed_acidity)

df |> arrange(desc(fixed_acidity))

df |> arrange(fixed_acidity, volatile_acidity)

select(data, column_1, column_2, column_3)

data |> select(column_1, column_2, column_3)

df |> select(quality)

df |> select(-quality)

df |> select(alcohol, quality)

df |> select(-alcohol, -quality)

names(df)

df |> select(chlorides:pH)

df |> select(-(chlorides:pH))  # note the brackets

df |> select(quality, everything())

data |> 
    mutate(
        new_column_name_1 = func_of_column_name_X,
        new_column_name_2 = func_of_column_name_Y,
        new_column_name_3 = func_of_column_name_Z
    )

new_df <- df %>%
    mutate(ratio_sulfur_dioxide = free_sulfur_dioxide / total_sulfur_dioxide) 

new_df %>%
    select(free_sulfur_dioxide, total_sulfur_dioxide, ratio_sulfur_dioxide)

data |> 
    summarise(
        summary_1 = summary_func_1(column_name),
        summary_2 = summary_func_2(column_name),
        summary_3 = summary_func_3(column_name)
    )

df |> 
    summarise(
        mean_alcohol = mean(alcohol, na.rm = TRUE)
    )

df |> 
    group_by(quality) |> 
    summarise(mean_alcohol = mean(alcohol, na.rm = TRUE))


library(tidyverse)

df <- read_csv(
    './ressource/wine_quality.csv',
    col_types = 'idddddddddddi'
)

df |> map_dbl(sum)  # Using a named function

df |> map_dbl(function(x) mean(x))  # Using an anonymous function

df |> map_dbl(~ max(.x))  # Using a formula

df |> 
    group_by(quality) |> 
    nest()

df |> 
    group_by(quality) |> 
    nest() |> 
    mutate(
        lin_reg = map(data, ~ lm(.x$fixed_acidity ~ .x$volatile_acidity))
    )


library(tidyverse)

df <- read_csv(
    './ressource/wine_quality.csv',
    col_types = 'idddddddddddi'
) |> 
    mutate(quality = as.factor(quality))

ggplot(
    data = df, # data
    aes(x = alcohol, y = fixed_acidity)  # aesthetic
) + 
  geom_point()  # layer

ggplot(
    data = df,  # data
    aes(x = alcohol)  # aesthetic
) + 
    geom_boxplot()  # layer

ggplot(
    data = df,  # data
    aes(x = alcohol, y = 0)  # aesthetic
) + 
    geom_boxplot() +  # layer 1
    geom_jitter()  # layer 2 

ggplot(
    data = df, 
    aes(
        x = alcohol, 
        y = fixed_acidity,
        colour = quality  # Change color
    )
) + 
    geom_point()

ggplot(
    data = df, 
    aes(
        x = alcohol, 
        y = fixed_acidity,
        shape = quality  # Change shape
    )
) + 
    geom_point()

ggplot(
    data = df, 
    aes(
        x = alcohol, 
        y = fixed_acidity,
        size = quality  # Change size
    )
) + 
    geom_point()

ggplot(
    data = df, 
    aes(
        x = alcohol, 
        y = fixed_acidity,
        colour = quality,  # Change color
        shape = quality,  # Change shape
        size = quality  # Change size
    )
) +  
    geom_point() 

ggplot(
    data = df,
    aes(
        x = alcohol,
        y = fixed_acidity
    )
) +
    geom_point() +
    xlab("Alcohol by volume") +  # Change x axis labels
    ylab("Fixed acidity") +  # Change y axis labels
    ggtitle("Wine quality data")  # Change title

ggplot(
    data = df,
    aes(
        x = alcohol,
        y = fixed_acidity,
        colour = quality
    )
) +
    geom_point() +
    labs(
        x = "Alcohol by volume",  # Change x axis labels
        y = "Fixed acidity",  # Change y axis labels
        title = "Wine quality data",  # Change title
        subtitle = "Scatterplot by quality"  # Change subtitle
    )

ggplot(
    data = df, 
    aes(
        x = alcohol, 
        y = fixed_acidity,
        colour = quality
    )
) + 
    geom_point() +
    theme_bw()  # Change to theme_bw

ggplot(
    data = df, 
    aes(
        x = alcohol, 
        y = fixed_acidity,
        colour = quality
    )
) + 
    geom_point() +
    theme_minimal()  # Change to theme_minimal


library(tidyverse)

email <- "name_surname@ul.ie"
str_detect(email, "^[a-z]*[.][a-z]*@ul.ie", negate = TRUE)

email <- "name.surname@ul.ie"
str_extract(email, "([a-z]*).([a-z]*)@ul.ie", group = 1)
str_extract(email, "([a-z]*).([a-z]*)@ul.ie", group = 2)

str_replace("colorful", "or", "our")

df <- read_csv(
    './ressource/wine_quality.csv',
    col_types = 'idddddddddddi'
) |> 
    mutate(quality = as.factor(quality))

ggplot(df, aes(x=quality, y=alcohol)) +
  geom_boxplot() +
  theme_bw()

ggplot(
    data = df,
    aes(x=fct_reorder(quality, alcohol, .fun = median), y=alcohol)
) +
  geom_boxplot() +
  theme_bw()

ggplot(
    data = df,
    aes(x=fct_infreq(quality), y=alcohol)
) +
  geom_boxplot() +
  theme_bw()

ggplot(
    data = df,
    aes(
        x=fct_collapse(
            quality,
            low = c("3", "4"),
            average = c("5", "6"),
            good = c("7", "8")
        ),
        y=alcohol
    )
) +
  geom_boxplot() +
  theme_bw()

today <- ymd("2023-09-21")

day(today)
month(today)
year(today)

next_course <- today + days(7)
next_course


