# Fortgeschrittene Programmierung WS 2014/15
# Hausübung 1 (due to 23.11.2014)
# Janek Thomas, Philipp J. Rösch

library(ggplot2)

### 3 

# load wtid_clean.rds
wtid_clean <- readRDS("wtid_clean.rds")

# check differneces between the data frames
all.equal(wtid_clean, wtid)
identical(wtid_clean, wtid)

### 3 a)

# set ratio according to description
ratio <- wtid_clean$p99 / wtid_clean$p995

#' input:   ratio: fraction of 99% quantile devided by 99.5% quantile
#' output:  estimator for parameter shape (ahat)
get_shape_ratio995 <- function(ratio){
  1 - log(2)/log(ratio)
}

# apply function get_shape_ratio995() and add it to wtid_clean
wtid_clean <- transform(wtid_clean, shape_99_995 = get_shape_ratio995(p99 / p995))

# plot estimator for France and United States
ggplot(data = subset(wtid_clean, country %in% c("France", "United States"))) +
    geom_line(aes(x = year, y = shape_99_995, color = country, lty = country)) +
    ylab("shape estimate (from P99 / P99.5)") + 
    theme_minimal(base_size=18)

### 3 b)

#' input:   p1:     first percentile
#'          p2:     second percentile
#'          factor: factor value in Pareto formula
#' output:  estimator for parameter shape (ahat)
get_shape_ratio <- function(p1, p2, factor){
  1 - log(factor) / log(p1 / p2)
}

# apply function get_shape_ratio() and add it to wtid_clean
wtid_clean <- transform(wtid_clean, 
                        shape_99_9999 = get_shape_ratio(p99, p9999, 100),
                        shape_99_999 = get_shape_ratio(p99, p999, 10),
                        shape_995_999 = get_shape_ratio(p995, p999, 5))

# scatterplot matrix
pairs(~ shape_99_995 + shape_99_9999 + shape_99_999 + shape_995_999, 
      data = wtid_clean, col = rgb(0, 0, 0, 0.1), pch = 20,
      xlim = c(2, 12), ylim = c(2, 12), cex = 0.3, cex.labels = 1.3)
