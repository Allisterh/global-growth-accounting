library(tidyverse)
library(openxlsx)

# Importing and cleaning databases

## Download Penn World Table (v 10.0)
data_raw <- 
  read.xlsx("https://www.rug.nl/ggdc/docs/pwt100.xlsx", sheet = 3) %>% as_tibble() %>% 
  select(country, year, y = rgdpna, l = emp, pop, k = rnna, labsh) %>% na.omit()

## Download ISO country codes
countries <- 
  read_csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv") %>% 
  select(country = name, countrycode = `alpha-3`, region, sub_region = `sub-region`)

## Join dataframes
data <- 
  data_raw %>% 
  left_join(countries) %>% 
  select(country, countrycode, region, sub_region, everything())

## Which countries don't have enough data for a 1979 to 2019 analysis?
remove <- 
  data %>% 
  filter(year %in% c(1979:2019)) %>% 
  group_by(country) %>% 
  count(country) %>% 
  filter(n < 40) %>% 
  select(country)

## Remove 'em
data %<>% 
  filter(year %in% c(1979:2019)) %>% 
  anti_join(remove) %>% 
  filter(countrycode != "NA")

## Group by regions
regions <-
  data %>%
  group_by(region, year) %>%
  summarise_at(vars(y:labsh), mean, na.rm = TRUE)

## Filter Guatemala
gtm <- 
  data %>% 
  filter(country == "Guatemala") %>% 
  select(!region) %>% 
  rename(region = country)

## Join regions and Guatemala
data <- 
  regions %>% 
  rbind(gtm) %>% 
  select(!c(countrycode, sub_region))

## Save the dataset
write_csv(data, "data.csv")
