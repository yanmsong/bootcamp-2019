gapminder = read.csv(here::here("data", "gapminder5.csv"))
str(gapminder)

gapminder$country <- as.character(gapminder$country)
gapminder$continent <- as.character(gapminder$continent)
str(gapminder)

obs <- 1:nrow(gapminder)
for (i in obs) { 
    gapminder[i, "gdp"] <- gapminder[i, "pop"] * gapminder[i, "gdpPercap"]
}

for (i in obs) {
    gapminder[i, "log_gdpPercap"] = log(gapminder[i, "gdpPercap"])
    gapminder[i, "log_pop"] = log(gapminder[i, "pop"])
}

gapminder$vec_log_gdpPercap <- log(gapminder$gdpPercap)
all(gapminder$vec_log_gdpPercap == gapminder$log_gdpPercap)

years = unique(gapminder$year)
for (i in years) {
    mean_le = mean(gapminder$lifeExp[gapminder$year == i], na.rm=T)
    print(paste0(i, ": ", mean_le))
}

continents = unique(gapminder$continent)
for (i in continents) {
    mean_le = mean(gapminder$lifeExp[gapminder$continent == i], na.rm=T)
    print(paste0(i, ": ", mean_le))
}

for (i in continents) {
    print(paste0("Continent: ", i))
    for (j in years) {
        mean_le <- mean(gapminder$lifeExp[gapminder$continent == i & 
                                              gapminder$year == j], 
                        na.rm = T)
        print(paste0(j, ": ", mean_le))
    }
}


for (i in continents) {
    print(paste0("Continent: ", i))
    for (j in years) {
        sd_le <- sd(gapminder$lifeExp[gapminder$continent == i & 
                                          gapminder$year == j], 
                    na.rm = T)
        print(paste0(j, ": ", sd_le))
    }
}

# apply(matrix, 1 = row or 2 = column, function)
vars <- gapminder[, c("lifeExp", "pop", "gdpPercap")]
apply(vars, 2, mean)

# lapply(vector, function)
lapply(gapminder, mean)

sapply(years, function(x) mean(gapminder$lifeExp[gapminder$year == x]))

i <-  1987
while (i <= 2002) {
    sd_le <- sd(gapminder$lifeExp[gapminder$year == i])
    print(paste0(i, ": ", sd_le)
    )
    i <- i + 1
}
