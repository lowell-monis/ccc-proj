library(tidyverse)

ccc1 <- read.csv("data/ccc_compiled_20172020.csv")
ccc1 <- ccc1 %>% 
  select(-starts_with("source"))

ccc2 <- read.csv("data/ccc_compiled_20212024.csv")
ccc2 <- ccc2 %>% 
  select(-starts_with("source"))
ccc <- bind_rows(ccc1, ccc2)

ccc
