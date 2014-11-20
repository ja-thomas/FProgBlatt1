r <- readRDS("/Users/philipprosch/Desktop/wtid_clean.rds")
str(r)
all.equal(r, w)
identical(r, w)

ratio <- r$p99 / r$p995

get_shape_ratio995 <- function(ratio){
  1- log(2 - ratio)
}

w$shape_99_995 <- transform(w, shape_99_995 = get_shape_ratio995(p99 / p995))

