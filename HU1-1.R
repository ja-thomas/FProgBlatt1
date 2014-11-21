# Fortgeschrittene Programmierung WS 2014/15
# Hausübung 1 (due to 23.11.2014)
# Janek Thomas, Philipp J. Rösch

### 1 a)

# reads data from csv
wtid <- read.csv("wtid-20141002.csv", 
                 skip = 1, header = TRUE)

### 1 b)

# keeps interested variables and rename them
keeps <- c("Country", "Year", "P99.income.threshold", 
           "P99.5.income.threshold", "P99.9.income.threshold", 
           "P99.99.income.threshold")
wtid <- wtid[keeps]
colnames(wtid) <- c("country", "year", "p99", "p995", "p999", "p9999")

# gives 99.99% quantile from the United States for 1981 and 2001
# single output
wtid[wtid$country == "United States" & wtid$year == "1981", "p999"]
wtid[wtid$country == "United States" & wtid$year == "2001", "p999"]

#combined output with different methods
wtid[wtid$country == "United States" & (wtid$year == "1981" | wtid$year == "2001"), "p999"]
subset(wtid, country == "United States" & (wtid$year == "1981" | wtid$year == "2001"), 
       select = "p999", drop = TRUE)

### 1 c)

# remove countries with less than 20 complete observations
complete_cases_indicator <- complete.cases(wtid[ ,3:6])
country_indicator <- tapply(complete_cases_indicator, wtid$country, function(x) sum(x) >= 20)
remaining_countries <- names(country_indicator[country_indicator == TRUE])
wtid <-wtid[wtid$country %in% remaining_countries, ]

# drop levels from removed countries
wtid <- droplevels(wtid)
levels(wtid$country)
