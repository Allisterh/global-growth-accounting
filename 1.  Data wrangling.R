if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, magrittr, openxlsx)

# Importing and cleaning databases ----------------------------------------

## Download Penn World Table (v 10.0)
penn <- 
  read.xlsx("https://www.rug.nl/ggdc/docs/pwt100.xlsx", sheet = 3) %>% as_tibble() %>% 
  select(country, year, y = rgdpna, l = emp, pop, k = rnna, labsh) %>% na.omit()

## Download ISO country codes
countrycodes <- 
  read_csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv") %>% 
  select(country = name, countrycode = `alpha-3`, region, subregion = `sub-region`)

## Join dataframes
data_raw <- 
  penn %>% 
  left_join(countrycodes) %>% 
  select(country, countrycode, region, subregion, everything())

# Dataframe 1: By region and Guatemala ------------------------------------

## Which countries don't have enough data for a 1979 to 2019 analysis?
remove <- 
  data_raw %>% 
  filter(year %in% c(1979:2019)) %>% 
  group_by(country) %>% 
  count(country) %>% 
  filter(n < 40) %>% 
  select(country)

## Remove 'em
countries <-
  data_raw %>%
  filter(year %in% c(1979:2019)) %>%
  anti_join(remove) %>%
  filter(countrycode != "NA")

## Group by regions
by_regions <-
  countries %>%
  filter(country != "Guatemala") %>% 
  group_by(region, year) %>%
  summarise_at(vars(y:labsh), mean, na.rm = TRUE)

## Filter Guatemala
gtm <- 
  countries %>% 
  filter(country == "Guatemala") %>% 
  select(!region) %>% 
  rename(region = country)

## Bind dataframes
regions <- 
  by_regions %>% 
  rbind(gtm) %>% 
  select(!c(countrycode, subregion))

## Translate regions names
regions %<>% 
  mutate(
    region = as.factor(region),
    region = fct_recode(region,
                             "África" = "Africa",
                             "América" = "Americas",
                             "Europa" = "Europe",
                             "Asia" = "Asia",
                             "Oceanía" = "Oceania",
                             "Guatemala" = "Guatemala")
    ) %>% 
  ungroup()

## Save the dataset
write_csv(regions, "regions.csv")
