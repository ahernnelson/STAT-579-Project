library(tidyverse)

all_data <- read_csv("Datasets//all_data.csv")

all_data %>%
    filter(year == 2017) %>%
    group_by(csr) %>%
    summarise(freq = n()/nrow(.)) %>%
    ggplot(aes(x = as.factor(csr), y=freq)) + 
    geom_col(position = "dodge", col = "black") +
    scale_y_continuous(labels = scales::percent)


all_data %>%
    mutate(rent_control = case_when(csr %in% c(1,2,12) ~ "Non Rental Property",
                                    csr %in% c(5, 20, 21, 22, 23, 85, 86, 95) ~ "Other Regulation",
                                    csr %in% c(30, 31, 90) ~ "Rent Controlled or Stabilized",
                                    csr == 80 ~ "Unregulated")) -> rc_data
rc_data %>%
    filter(year == 2017) %>%
    group_by(rent_control) %>%
    summarise(freq = n()/nrow(.)) %>%
    ggplot(aes(x = as.factor(rent_control), y=freq)) + 
    geom_col(position = "dodge", col = "black") +
    scale_y_continuous(labels = scales::percent)
plotly::ggplotly()


rc_data %>%
    filter(year == 2017) %>%
    group_by(rent_control) %>%
    summarise(avg_pqi = mean(pqi)) %>%
    ggplot(aes(x = rent_control, y=avg_pqi)) + 
    geom_col(position = "dodge", col = "black") 
plotly::ggplotly()

rc_data %>%
    filter(year == 2017) %>%
    ggplot(aes(x = rent_control, y=log(pqi+1))) + 
    geom_boxplot(col = "black", fill = "gray", alpha = .5) + 
    theme_bw()  +
    xlab("\nRental Status") +
    ylab("Log Transformed PQI\n") +
    ggtitle("Variation in PQI by Rent-Regulation Status")

    


rc_data %>%
    filter(year == 2017) %>%
    group_by(csr) %>%
    summarise(avg_pqi = mean(pqi)) %>%
    ggplot(aes(x = csr, y=avg_pqi)) + 
    geom_col(position = "dodge", col = "black") 
plotly::ggplotly()
                                 