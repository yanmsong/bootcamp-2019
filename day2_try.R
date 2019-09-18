library(here)
generation <- read.csv(here::here("data/ca_energy_generation.csv"), stringsAsFactors=F)
imports <- read.csv(here::here("data/ca_energy_imports.csv"), stringsAsFactors=F)
str(generation)

# deal with date-time data: use the lubridate package
# readr::read_csv often detect and convert datetime variables when importing
library(lubridate)
generation$datetime <- as_datetime(generation$datetime)
class(generation$datetime)
imports$datetime <- as_datetime(imports$datetime)
class(imports$datetime)

library(reshape2)
long_gen <- melt(generation, id.vars = "datetime",
                 variable.name = "source",
                 value.name = "usage")
head(long_gen)

head(long_gen[order(long_gen$datetime), ])
merged_energy <- merge(generation, imports, by = "datetime")
dim(merged_energy)

long_merged_energy <- melt(merged_energy, id.vars = "datetime",
                           variable.name = "source",
                           value.name = "usage")
head(long_merged_energy)

# dplyr, and data.table, are two packages used to process data in tabular form
library(tidyverse)

# SELECT: subset variables
tmp <- select(merged_energy, biogas, biomass, geothermal, solar)
names(tmp)
tmp <- select(merged_energy, -biogas, -biomass, -geothermal, -solar)
names(tmp)
tmp <- select(merged_energy, contains("hydro"), starts_with("bio"))
names(tmp)

# FILTER: subset observations based on conditions
tmp <- filter(merged_energy, imports > 7000)
nrow(tmp)
tmp <- filter(merged_energy, imports > 7000, natural_gas < 7000)
nrow(tmp)

# MUTATE: create new variables
tmp <- mutate(long_merged_energy, log_usage = log(usage))
head(tmp)
tmp <- mutate(long_merged_energy, log_usage = log(usage), usage2 = usage^2, usage3 = usage^3)
head(tmp)

# SUMMARIZE: reduce observations to a single value based on functions - mean, sum, sd, min, max, etc.
summarize(long_merged_energy, total = sum(usage, na.rm = T))
summarize(long_merged_energy, mean_cons = mean(usage, na.rm = T))

long_merged_energy %>% 
    filter(source == "geothermal") %>% 
    select(-datetime) %>% 
    mutate(log_usage = log(usage)) %>% 
    summarize(mean_log_usage = mean(log_usage, na.rm = T))

# %>% operator lets you chain together functions
merged_energy %>% 
    select(contains("hydro")) %>% 
    mutate(total_hydro = rowSums(., na.rm = T)) %>%
    summarize(mean_hydro = mean(total_hydro, na.rm = T))

long_merged_energy %>% 
    group_by(source) %>% 
    summarize(sum_usage = sum(usage, na.rm = T))            

# short merged energy
merged_energy %>% 
    select(datetime, contains("hydro"), contains("bio")) %>% 
    melt(id.vars = "datetime",
         variable.name = "source",
         value.name = "usage") %>% 
    group_by(source) %>% 
    summarize(mean_usage = mean(usage, na.rm = T))      

# long merged energy
long_merged_energy %>% 
    filter(source %in% c("large_hydro", "small_hydro", "biogas", "biomass")) %>% 
    group_by(source) %>% 
    summarize(mean_usage = mean(usage, na.rm = T))   


