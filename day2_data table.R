# import data.table library
library(data.table)

data_file <- here::here("data", "ca_energy_generation.csv")

# read in two versions of data, one as a data.frame and one as a data.table
generation_df <- read.csv(data_file, stringsAsFactors = F)

generation_dt <- fread(data_file)

str(generation_df)
str(generation_dt)

# dt[i, j, by]: i is row filter, j is column operations, by is groupby
generation_dt[wind > 4400]
generation_dt[wind > 4400 & mday(datetime) == 7]
generation_dt[natural_gas <= 5000 & large_hydro > 2000]
generation_dt[coal > 10 & solar > median(solar)]

# := is in place, .() is export data
generation_dt[,newcol:= 3*wind + solar*biogas/2]
generation_dt[,.(newcol = 3*wind + solar*biogas/2)]
generation_dt[,newcol := NULL]

generation_dt[,total_hydro := small_hydro + large_hydro]
generation_dt[,.(mean(nuclear), mean(biogas))]
generation_dt[solar == 0, .(datetime, total_thermal = natural_gas + coal)]

generation_dt[,median(solar), by = hour(datetime)]
generation_dt[solar > 0, max(natural_gas), by = mday(datetime)]

generation_dt[,.N]
generation_dt[,.I]
