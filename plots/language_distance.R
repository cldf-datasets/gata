library(readr)
library(dplyr)
library(tidyr)


data <- read_csv('../raw/gata_raw.csv')

ind_years <- data %>% group_by(Language_ID, Year) %>% count() %>% select(-n)
ind_years["grammar"] <- rep(c("first", "second"),times=51)

ind_years <- ind_years %>% pivot_wider(names_from=grammar, values_from=Year) 


ind_years %>% mutate(diff=get("second")-get("first")) %>% arrange(diff) %>% 
  filter(diff>=100)

