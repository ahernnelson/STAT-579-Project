#library(tidyverse)
library(scales)
library(ggsci)

labs = seq(1990,2017,3)

all_data %>% group_by(year) %>% 
    summarise(p_99 = quantile(pqi,.99),
              p_95 = quantile(pqi,.95),
              p_90 = quantile(pqi,.90),
              p_75 = quantile(pqi,.75),
              Mean = mean(pqi),
              Median = median(pqi)) %>% 
    rename_at(vars(c(p_99,p_95,p_90,p_75)), ~paste0(c(99,95,90,75),"th Percentile")) %>%
    gather(line, value, -one_of(c("year"))) %>%
    mutate(line=factor(line,levels=c(paste0(c(99,95,90,75),"th Percentile"),"Mean","Median"))) %>%
    ggplot(aes(x=year, y=value, col =line)) + 
    geom_line(size=1) +
    scale_color_d3(name="") + 
    theme_bw(base_size = 14) + 
    scale_x_continuous(breaks = labs, labels = c("",labs[-1])) + 
    ylab("Index Score") +
    ggtitle("Figure 4: Index Trends 1991-2017") + xlab("") 

#ggsave("index_trends.png")
