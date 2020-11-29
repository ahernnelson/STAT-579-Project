library(gganimate)


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
    geom_boxplot(alpha = .4) + 
    geom_smooth(data = log_means, 
                aes(group=fill_c, y=m, fill = fill_c, lty = "Mean Log PQI"), 
                col ="black") +
    theme_bw(base_size = 14) +
    xlab("") +
    ylab("Log-Scaled PQI") +
    ggtitle("Figure 5: PQI in Rented vs. Owned Units") +
    scale_linetype_manual(name = "Mean Log PQI", values = "dotted") +
    theme(legend.position = "top", legend.title = element_blank())

all_data %>%
    #filter(year ==2017) %>%
    mutate(Rent = if_else(mgrent > 9998, "Owner Occupied", "Renter Occupied")) %>%
    ggplot(aes(x=log(pqi + 1))) +
    geom_density(alpha = .5, aes(fill = Rent)) + 
    theme_bw(base_size = 14) +
    labs(title="Log PQI Distribution: {floor(frame_time)}") +
    transition_time(year) + 
    ease_aes("linear") -> dist_g

animate(dist_g)

