library(tidyverse)
library(scales)
library(ggsci)

all_data %>% group_by(year) %>% summarise(`Index > 0` = sum(pqi > 0)/n(), 
                                          `Index of 1-10` = sum(pqi %in% 1:10)/n(),
                                          `Index of 11-20`= sum(pqi %in% 11:20)/n(),
                                          `Index > 20` = sum(pqi>20)/n()) %>% 
    gather(Group, Percent, -one_of("year")) %>% 
    mutate(Group=factor(Group, levels = c("Index > 0", "Index of 1-10", "Index of 11-20", "Index > 20")))%>%
    ggplot(aes(x=year, y=Percent)) + 
    geom_col(aes(fill=Group), position="dodge") + 
    facet_wrap(facets = ~Group, ncol = 4) + 
    scale_x_reverse(breaks = seq(1990,2017,3)) + 
    scale_y_continuous(labels = percent) + 
    coord_flip() + 
    xlab("") + 
    ylab("") +
    ggtitle("Figure 3: Percent of Units With Quality Problems 1991 - 2017") + 
    theme_bw(base_size = 14) + 
    theme(legend.position="none")

#ggsave("percent_units_pqi.png")
