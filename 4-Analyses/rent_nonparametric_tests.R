library(tidyverse)

all_data <- read_csv("Datasets//all_data.csv")


all_data %>%
    mutate(rent_control = case_when(csr %in% c(1,2,12) ~ "Non Rental Property",
                                    csr %in% c(5, 20, 21, 22, 23, 85, 86, 95) ~ "Other Regulation",
                                    csr %in% c(30, 31, 90) ~ "Rent Controlled or Stabilized",
                                    csr == 80 ~ "Unregulated")) -> rc_data

# Equal Variance ----------------------------------------------------------

library(car)
leveneTest(log(pqi+1) ~ rent_control, data = rc_data)




# Wilcoxon Sum Rank -------------------------------------------------------

wilcox.test(pqi ~ if_else(csr %in% c(1, 2, 12), 0, 1), data = all_data, 
            correct = F, alternative = "less")
    # Owner occupied have lower pqi on average than rental or publcily subsidize units.

# Kruskal-Wallis ----------------------------------------------------------

#kruskal.test(pqi ~ csr, data = rc_data)
kruskal.test(pqi ~ rent_control, data = rc_data)



