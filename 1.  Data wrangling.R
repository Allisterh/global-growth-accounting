if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, magrittr, openxlsx)

# Importing and cleaning databases ----------------------------------------

## Penn World Table (v 10.0)
pennworld_raw <- 
  read.xlsx("https://www.rug.nl/ggdc/docs/pwt100.xlsx", sheet = 3) %>% as_tibble() %>% 
  select(country, countrycode, year, y = rgdpna, l = emp, pop, k = rnna, labsh) %>% na.omit()

## ISO country codes and regions
countrycodes <- 
  read_csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv") %>% 
  select(countrycode = `alpha-3`, region, subregion = `sub-region`)

## Join data frames
pennworld <- 
  pennworld_raw %>% 
  left_join(countrycodes) %>% 
  select(country, countrycode, region, subregion, everything())

## Our World in Data: https://ourworldindata.org/extreme-poverty
poverty_raw <- 
  read_csv("worldbank_poverty.csv") %>%
  rename(
    country = Entity,
    countrycode = Code,
    year = Year,
    pov = `$1.90 per day - share of population below poverty line`
  )

# Data frame 1: Growth by contintent (+ Guatemala) -------------------------

## Which countries don't have enough data for a 1984 to 2019 analysis?
remove <- 
  pennworld %>% 
  filter(year %in% c(1984:2019)) %>% 
  group_by(country) %>% 
  count(country) %>% 
  filter(n < 35) %>% 
  select(country)

## Remove 'em
countries <-
  pennworld %>%
  filter(year %in% c(1984:2019)) %>%
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

## Bind data frames
regions <- 
  by_regions %>% 
  rbind(gtm) %>% 
  select(!c(countrycode, subregion))

## Translate regions names and add id's
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
  mutate(id = cur_group_id()) %>% 
  select(id, everything()) %>% 
  ungroup()

## Save dataset
write_csv(regions, "regions_growth.csv")

# Data frame 2: Guatemala --------------------------------------------------

## Filter Guatemala
guatemala <- 
  pennworld %>% 
  filter(country == "Guatemala" & year >= 1954) %>% 
  select(country, year:labsh)

## Save dataset
write_csv(guatemala, "guatemala_growth.csv")

# Data frame 3: Countries --------------------------------------------------

write_csv(countries, "countries_growth.csv")

# Data frame 4: Global extreme poverty -------------------------------------

## Join data frames
poverty <- 
  poverty_raw %>% 
  left_join(countrycodes) %>% 
  select(country, countrycode, region, subregion, everything())

## Which countries don't have enough data for a 1984 to 2019 analysis?
remove <- 
  poverty %>% 
  filter(year %in% c(1984:2019)) %>% 
  group_by(country) %>% 
  count(country) %>% 
  arrange(n) %>% 
  filter(n < 35) %>% 
  select(country)

## Remove 'em
countries <-
  poverty %>%
  filter(year %in% c(1984:2019)) %>%
  anti_join(remove) %>%
  filter(countrycode != "NA")

## Group by regions
by_regions <-
  countries %>%
  filter(country != "Guatemala") %>% 
  group_by(region, year) %>%
  summarise_at(vars(pov), mean, na.rm = TRUE)

## Filter Guatemala
gtm <- 
  countries %>% 
  filter(country == "Guatemala") %>% 
  select(!region) %>% 
  rename(region = country)

## Bind data frames
regions <- 
  by_regions %>% 
  rbind(gtm) %>% 
  select(!c(countrycode, subregion))

## Translate regions names and add id's
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
  mutate(id = cur_group_id()) %>% 
  select(id, everything()) %>% 
  ungroup()

## Save dataset
write_csv(regions, "regions_poverty.csv")
