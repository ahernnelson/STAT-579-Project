#library(tidyverse)
library(gganimate)
library(gifski)

#all_data <- readr::read_csv("Datasets//all_data.csv")

all_data %>% group_by(year, pqi) %>% summarise(count = n()) %>% 
    mutate(percent = count/sum(count)) %>%
    ggplot(aes(x=pqi,y=percent)) + 
    geom_segment(aes(yend=0, xend=pqi), alpha = .9) +
    geom_point(size=4, color="lightblue") +
    geom_line(alpha=.5,size=1.5, col="lightblue") + 
    scale_y_continuous(labels=scales::percent, breaks = (0:6)/10) + 
    scale_x_continuous(breaks=seq(0,56,4)) + 
    xlab("Index Score") + 
    ylab("") + 
    labs(col="") + 
    theme_bw(base_size = 14) +
    labs(title="Figure 1: PQI Frequency Distribution: {floor(frame_time)}") +
    transition_time(year) + 
    ease_aes("linear") + 
    exit_fade() + 
    enter_fade() -> 
    g

animate(g)

#animate(g, height = 4.5, width = 6, units = "in", res = 180)

# anim_save(filename = "3-Graphs//freq_dist.gif")


