#library(tidyverse)
library(scales)
library(ggsci)

sec_labels = paste0(c(69:73),"%")
labs <- seq(1990, 2017, 3)

all_data %>% 
    mutate(Rent = csr %!in% c(1,2,12)) %>%
    group_by(year) %>% 
    summarise(`Rent Share` = mean(Rent), avg_pqi=mean(pqi)) %>%
    mutate(scale_rent = rescale(`Rent Share`, to=range(avg_pqi))) %>% #-> temp_df
    ggplot(aes(x=year)) + 
        geom_line(size = 1, aes(y=avg_pqi,lty="Avg PQI")) + 
        geom_line(aes(y=scale_rent, lty="Rent Share"), size=1) +
        scale_x_continuous(breaks = labs) + 
        xlab("") + 
        ylab("Mean PQI") +
        scale_y_continuous(sec.axis=sec_axis(~., name="Percent Non-Owner-Occupied Units\n", labels = sec_labels)) +
        theme_bw(base_size = 14) + 
        ggtitle("Share of Non-Owner-Occupied Units with Average PQI ") +
        scale_linetype_manual(breaks=c("Avg PQI","Rent Share"), values = c("solid","dotted")) +
        theme(legend.position = "top", legend.title = element_blank()) 

#ggsave("rent_share_pqi.png")