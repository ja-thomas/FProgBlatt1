# Hausübung 1

### 1 a
wtid <- read.csv("wtid-20141002.csv", 
                 skip=1, header=TRUE)
View(wtid)
str(wtid)

### 1 b

keeps <- c("Country", "Year", "P99.income.threshold", 
           "P99.5.income.threshold", "P99.9.income.threshold", 
           "P99.99.income.threshold")
wtid <- wtid[keeps]
str(wtid)

colnames(wtid) <- c("country","year", "p99", "p995", "p999", "p9999")

wtid[wtid$country=="United States" & wtid$year=="1981", "p999"]
wtid[wtid$country=="United States" & wtid$year=="2001", "p999"]

#NAs_per_row <- apply(wtid[,3:6], 1, function(x) sum(is.na(x)))
#wtid$NAs_per_row <- NAs_per_row

# Länder rauslöschen: 

###
wtid$cc <- complete.cases(wtid[,3:6])
x <- tapply(wtid$cc, wtid$country, function(x) sum(x)>=20)
str(x)

wtid[x==TRUE]

cnames <- names(x[x==TRUE])

wtid$country %in% cnames

w <-wtid[wtid$country %in% cnames,]

w <- droplevels(w)
levels(w$country)
