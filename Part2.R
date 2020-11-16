# install.packages("tidyverse")
library(tidyverse)

### Clean the date to generate new date for analysis
data2017 <- read.csv("Datasets/NYCHVS 2017 Occupied File for ASA Challenge_CSV.csv")
head(data2017)
dataall <- read.csv("Datasets/all_data.csv")
data2017_all <- dataall %>% filter(year == 2017)
data2017 <- data2017[-1,]
New_date2017 <- cbind(data2017, data2017_all)

### Change variable type to numeric
data2017$X_1f
New_data_regression <- New_date2017[,-c(254, 252)] %>% select(X_1b,X_1c,X_1f,X_1e,X_2,X_3, X_4a, X_5, X_6, X_12b, pqi, X_14)
New_data_regression$X_1c <- as.numeric(New_data_regression$X_1c)
New_data_regression$X_4a <- as.numeric(New_data_regression$X_4a)

### Run regression for variable selection
model <- lm(pqi ~ X_1b + X_1c +X_1f + X_1e + X_2 + X_3 + X_4a + X_5 + X_6  + X_12b + X_14,data = New_data_regression)
summary(model)

### Form the regression, we found the variable X_1b, X_1c, X_1f, X_1e, X_2, X_3, X_4a, X_6 are statistically significant for the PQI.

### The boxplot for variable X_1f
New_data_regression %>% ggplot() + geom_boxplot(aes(x =X_1f, y=pqi)) + 
    scale_x_discrete(labels=c("1" = "White", "2" = "Black or African American","3"="American Indian or Alaska Native",
                              "4"="Chinese","5"="Filipino","6"="Korean",
                              "7"="Asian Indian,Pakistani, or Bangladesh", "8"="Other Asian",
                              "9"="Native Hawaiian or Pacific Islander","10" = "More Races or Other Race ")) +
    xlab("Householder's Race") + coord_flip()

### The boxplot for variable X_1e
New_data_regression %>% ggplot() + geom_boxplot(aes(x =X_1e, y=pqi)) +
    scale_x_discrete(labels=c("1" = "No", "2" = "Puerto Rican","3"="Dominican",
                                            "4"="Cuban","5"="South/Central American","6"="Mexican, Mexican-American, Chicano",
                                            "7"="Other Spanish/Hispanic")) +
    xlab("Householder of Spanish/Hispanic") + coord_flip()

### The boxplot for variable X_2
New_data_regression %>% ggplot() + geom_boxplot(aes(x = X_2, y=pqi)) +
    scale_x_discrete(labels=c("1" = "None", "2" = "1 or more persons","8"="Person number not reported ",
                              "9"="Temporary residence status not reported")) +
    xlab("Number of Persons from Temporary Residence") + coord_flip()

### The boxplot for variable X_3
New_data_regression %>% ggplot() + geom_boxplot(aes(x = X_3, y=pqi)) +
    scale_x_discrete(labels=c("1"="Always lived in this unit",
                              "2"="Other unit in same building",
                              "3"="Bronx",
                              "4"="Brooklyn",
                              "5"="Manhattan",
                              "6"="Queens",
                              "7"="Staten Island",
                              "8"="NY, NJ, Connecticut",
                              "9"="Other state or Puerto Rico",
                              "10"="Dominican Republic",
                              "11"="Caribbean (other than Puerto Rico or Dom. Rep.)",
                              "12"="Mexico, Central America, South America, Canada",
                              "13"="Armenia, Azerbaijan, Belarus, Estonia, Georgia, Kazakhstan, Kyrgyzstan, Latvia, Lithuania,
                              Moldova, Russia, Tajikistan, Turkmenistan, Ukraine, Uzbekistan, or other European countries",
                              "14"="All other countries",
                              "98"="Not reported")) +
    xlab("Most Recent Place Householder Lived or 6 Months or More") + coord_flip()

### The boxplot for variable X_6
New_data_regression %>% ggplot() + geom_boxplot(aes(x = X_6, y=pqi)) +
    scale_x_discrete(labels=c("1"="Change in employment status",
                              "2"="Looking for work",
                              "3"="Commuting reasons",
                              "4"="To attend school",
                              "5"="Other financial/employment reason",
                              "6"="Needed larger house or apartment",
                              "7"="Widowed, separated/divorced, or family decreased",
                              "8"="Newly married",
                              "9"="Moved to be with or closer to relatives",
                              "10"="Wanted to establish separate household",
                              "11"="Other family reason",
                              "12"="Wanted this neighborhood/better neighborhood services",
                              "13"="Other neighborhood reason",
                              "14"="Wanted to own residence",
                              "15"="Wanted to rent residence",
                              "16"="Wanted greater housing affordability",
                              "17"="Wanted better quality housing",
                              "18"="Evicted, displaced, or harassment by landlord",
                              "19"="Other housing reason",
                              "20"="Any other reason",
                              "98"="Not reported",
                              "99"="Not applicable (moved in 2013 or earlier)")) +
    xlab("Reason for Householder Moving") + coord_flip()


### The density plot for X_1c
New_data_regression$X_1c_c <- New_data_regression$X_1c %>% cut(breaks=c(15, 22, 30, 50, 65, 85))
New_data_regression %>% ggplot() + geom_density(aes(x=pqi, fill = X_1c_c)) + scale_fill_discrete(name="Age Group",
                                                                                                 labels=c("15 to 22", "22 to 30", "30 to 50","50 to 65","65 to 85"))

### The histogram plot for X_4a
New_data_regression$X_4a_c <- New_data_regression$X_4a %>% cut(breaks=c(1950, 1975, 2000, 2010, 2020))
New_data_regression %>% ggplot() + 
    geom_histogram(aes(x=pqi, fill = X_4a_c), alpha = 0.5, position= "dodge") + scale_fill_discrete(name="Year To Move In",
                                                                                                labels=c("1950 to 1975", "1975 to 2000", "2000 to 2010","2010 to 2020"))
