#library(tidyverse)
all_data %>%
    mutate(rent_control = case_when(csr %in% c(1,2,12) ~ "Non-Rental Property",
                                    csr %in% c(5, 20, 21, 22, 23, 85, 86, 95) ~ "Other Regulation (Subsidized)",
                                    csr %in% c(30, 31, 90) ~ "Rent Controlled or Stabilized",
                                    csr == 80 ~ "Unregulated")) %>%
    mutate(rent_control = factor(rent_control, levels = c("Non-Rental Property", 
                                                          "Unregulated",
                                                          "Rent Controlled or Stabilized",
                                                          "Other Regulation (Subsidized)"))) %>%
    filter(year == 2017) %>%
    ggplot(aes(x = rent_control, y=log(pqi+1))) + 
    geom_boxplot(varwidth = T, col = "black", fill = "gray", alpha = .5) + 
    theme_bw(base_size = 14)  +
    xlab("\nRental Status") +
    ylab("Log Transformed PQI\n") +
    ggtitle("Figure 4: Variation in PQI by Rent-Regulation Status (2017)") ->
    fig_4

#ggsave("rent_regulate.png")
