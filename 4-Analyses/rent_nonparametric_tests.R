library(tidyverse)

all_data <- read_csv("Datasets//all_data.csv")


all_data %>%
    filter(year == 2017)
    mutate(rent_control = case_when(csr %in% c(1,2,12) ~ "Non Rental Property",
                                    csr %in% c(5, 20, 21, 22, 23, 85, 86, 95) ~ "Other Regulation",
                                    csr %in% c(30, 31, 90) ~ "Rent Controlled or Stabilized",
                                    csr == 80 ~ "Unregulated")) -> rc_data

# Equal Variance ----------------------------------------------------------

library(car)
leveneTest(pqi ~ rent_control, data = rc_data)




# Wilcoxon Sum Rank -------------------------------------------------------

wilcox.test(pqi ~ if_else(rent_control == "Non Rental Property", 0, 1), data = rc_data, 
            correct = F, alternative = "less")
    # Owner occupied have lower pqi on average than rental or publcily subsidize units.


wilcox.test(pqi ~ case_when(rent_control == "Unregulated" ~ 0,
                            rent_control == "Rent Controlled or Stabilized" ~ 1,
                            TRUE ~ NA_real_), 
            data = rc_data, correct = F, alternative = "less")
# Kruskal-Wallis ----------------------------------------------------------

#kruskal.test(pqi ~ csr, data = rc_data)
kruskal.test(pqi ~ rent_control, data = rc_data)



