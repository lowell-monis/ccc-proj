# imports
library(tidyverse)
library(stringr)

# keyword finding function
filter_keywords <- function(data, text_column, keywords, ignore_case = TRUE) {
  pattern <- paste(keywords, collapse = "|")
  if(ignore_case) {
    data %>%
      filter(str_detect({{ text_column }}, regex(pattern, ignore_case = TRUE)))
  } else {
    data %>%
      filter(str_detect({{ text_column }}, pattern))
  }
}

# data set
ccc1 <- read.csv("data/ccc_compiled_20172020.csv")
ccc1 <- ccc1 %>% 
  select(-starts_with("source"))

ccc2 <- read.csv("data/ccc_compiled_20212024.csv")
ccc2 <- ccc2 %>% 
  select(-starts_with("source"))
ccc <- bind_rows(ccc1, ccc2)

View(ccc)

# extracting date

ccc <- ccc %>%
  separate_wider_delim(date,
                       delim="-",
                       names=c('yyyy', 'mm', 'dd'))

ccc_fl <- ccc %>%
  filter(as.numeric(yyyy)==2020, as.numeric(mm)>=5) %>%
  filter_keywords(claims, c('floyd', 'brutality', 'police', 'racism'))
View(ccc_fl)

ccc_fl %>%
  group_by(mm, dd) %>%
  ggplot(aes(x=dd,y=property_damage)) +
  geom_point()
 