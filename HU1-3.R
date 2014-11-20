r <- readRDS("wtid_clean.rds")
str(r)
all.equal(r, w)
identical(r, w)

ratio <- r$p99 / r$p995

get_shape_ratio995 <- function(ratio){
  1- log(2)/log(ratio)
}
r <- transform(r, shape_99_995 = get_shape_ratio995(p99 / p995))


library(ggplot2)

ggplot(data = subset(r, country %in% c("France","United States"))) +
    geom_line(aes(x=year, y=shape_99_995, color=country, lty=country)) +
    ylab("shape estimate (from P99 / P99.5)") + 
    theme_minimal(base_size=18)


get_shape_ratio <- function(p1, p2, factor){
  1 - log(factor) / log(p1/p2)
}

r <- transform(r , shape_99_9999 = get_shape_ratio(p99, p9999, 100),
                   shape_99_999 = get_shape_ratio(p99, p999, 10),
                   shape_995_999 = get_shape_ratio(p995, p999, 5))

pairs(~shape_99_995 + shape_99_9999 + shape_99_999 + shape_995_999, data = r,
      xlim = c(2,12), ylim = c(2,12), pch=20, cex=0.3, col=rgb(0, 0, 0, 0.1))





  