all_data %>%
    mutate(Rent = if_else(mgrent > 9998, "Owner Occupied", "Renter Occupied")) %>% 
    group_by(year, Rent) %>%
    summarise(m = mean(log(pqi+1))) %>%
    ungroup() %>%
    rename(fill_c = Rent) -> log_means

log_means

all_data %>%
    mutate(Rent = if_else(mgrent > 9998, "Owner Occupied", "Renter Occupied")) %>%
    ggplot(aes(x = as.factor(year), fill = Rent, y = log(pqi+1))) +
    geom_boxplot(varwidth = TRUE, alpha = .4) +
    geom_smooth(data = log_means, 
                aes(group=fill_c, y=m, fill = fill_c, lty = "Mean Log PQI"), 
                col ="black") +
    theme_bw(base_size = 14) +
    xlab("") +
    ylab("Log-Scaled PQI\n") +
    ggtitle("Figure 3: PQI in Rented vs. Owned Units") +
    scale_linetype_manual(name = "Mean Log PQI", values = "dotted") +
    theme(legend.position = "top", legend.title = element_blank()) ->
    fig_3


