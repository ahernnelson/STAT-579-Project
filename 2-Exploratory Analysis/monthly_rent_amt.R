library(tidyverse)

all_data <- read_csv("Datasets//all_data.csv")



all_data %>%
    filter(mgrent < 5995) %>%
    ggplot(aes(x=mgrent)) +
    geom_histogram()

all_data %>%
    filter(year == 2017,
    mgrent < 5995) %>%
    ggplot(aes(x=mgrent, y=pqi)) +
    geom_point()



all_data %>%
    filter(year == 2017,
           mgrent < 6000) %>%
    mutate(floor_rent = case_when(between(mgrent, 0, 999) ~ "0-1000",
                                  between(mgrent, 1000, 1999) ~ "999-1999",
                                  between(mgrent, 2000,2999) ~ "2000-2999",
                                  between(mgrent,3000,4999) ~ "3000-4999",
                                  between(mgrent, 5000,5995) ~ "5000-5995",
                                  )) %>%
    ggplot(aes(x=floor_rent, y= log(pqi+1))) +
    geom_boxplot()


