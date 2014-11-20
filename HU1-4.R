###a)

match_ratios <- function(p99, p995, p999, p9999){
  require(minqa)
  
  shape_start_value <- get_shape_ratio995(p99 / p995)
  
  bobyqa(shape_start_value, estimate_shape, 
         p99 = p99, p995 = p995, p999 = p999, p9999 = p9999)
  
}

estimate_shape <- function(shape, p99, p995, p999, p9999){
    loss(p99, p995, shape, 2) +
    loss(p995, p999, shape, 5) +
    loss(p99, p999, shape, 10) +
    loss(p99, p9999, shape, 100)
}



loss <- function(p1, p2, shape, factor){
  (((p1/p2)^(1-shape)) - factor)^2
}

match_ratios(10^6,2*10^6,10^7,10^8)

#b)

match_ratios <- function(p99, p995, p999, p9999, 
                         type=c("quadratic", "absolute", "relative")){
  require(minqa)
  
  type <- match.arg(type)
  
  shape_start_value <- get_shape_ratio995(p99 / p995)
  
  bobyqa(shape_start_value, estimate_shape, 
         p99 = p99, p995 = p995, p999 = p999, p9999 = p9999, type = type)
  
}


estimate_shape <- function(shape, p99, p995, p999, p9999, type){
    loss(p99, p995, shape, 2, type) +
    loss(p995, p999, shape, 5, type) +
    loss(p99, p999, shape, 10, type) +
    loss(p99, p9999, shape, 100, type)
}

loss <- function(p1, p2, shape, factor, type){
  
  if(is.na(p1) | is.na(p2)){
    warning("missing value in percentiles")
    return(0)
  }
  
  if(type == "quadratic"){
    (((p1/p2)^(1-shape)) - factor)^2
  }
  else if(type == "absolute"){
    abs((((p1/p2)^(1-shape)) - factor))
  }
  else{
    abs(log(((p1/p2)^(1-shape)) / factor))
  }
}


match_ratios(10^6,2*10^6,10^7,10^8, "quadratic")
match_ratios(10^6,2*10^6,10^7,10^8, "absolute")
match_ratios(10^6,2*10^6,10^7,10^8, "relative")

match_ratios(10^6, 2*10^6, NA, NA)
