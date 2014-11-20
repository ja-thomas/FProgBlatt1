###a)

match_ratios <- function(p99, p995, p999, p9999){
  require(minqa)
  
  shape_start_value <- get_shape_ratio995(p99 / p995)
  
  bobyqa(shape_start_value, estimate_shape, 
         p99 = p99, p995 = p995, p999 = p999, p9999 = p9999)
  
}

estimate_shape <- function(shape, p99, p995, p999, p9999){
    quantile_estimate_pareto(p99, p995, shape, 2)^2 +
    quantile_estimate_pareto(p995, p999, shape, 5)^2 +
    quantile_estimate_pareto(p99, p999, shape, 10)^2 +
    quantile_estimate_pareto(p99, p9999, shape, 100)^2
}



quantile_estimate_pareto <- function(p1, p2, shape, factor){
  ((p1/p2)^(1-shape)) - factor
}

match_ratios(10^6,2*10^6,10^7,10^8)

#b)

estimate_shape <- function(shape, p99, p995, p999, p9999){
  quantile_estimate_pareto(p99, p995, shape, 2)^2 +
    quantile_estimate_pareto(p995, p999, shape, 5)^2 +
    quantile_estimate_pareto(p99, p999, shape, 10)^2 +
    quantile_estimate_pareto(p99, p9999, shape, 100)^2
}

