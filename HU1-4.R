# Fortgeschrittene Programmierung WS 2014/15
# Hausübung 1 (due to 23.11.2014)
# JT, PJR

### 4

##a)

#' input:   99&, 99.5%, 99.9% and 99.99% quantile of one year/country 
#' combination
#' output:  estimated shape value and convergence informations
match_ratios <- function(p99, p995, p999, p9999){
  
  require(minqa)
  
  if(!is.vector(p99)){stop("99% quantile is not a correct format")}
  if(!is.vector(p995)){stop("99.5% quantile is not a correct format")}
  if(!is.vector(p999)){stop("99.9% quantile is not a correct format")}
  if(!is.vector(p9999)){stop("99.99% quantile is not a correct format")}
  
  if(!is.numeric(p99)){stop("99% quantile is not numeric")}
  if(!is.numeric(p995)){stop("99.5% quantile is not numeric")}
  if(!is.numeric(p999)){stop("99.9% quantile is not numeric")}
  if(!is.numeric(p9999)){stop("99.99½ quantle is not numeric")}
  
  if(is.na(p99)){stop("99% quantile is missing")}
  if(is.na(p995)){stop("99.5% quantile is missing")}
  if(is.na(p999)){stop("99.9% quantile is missing")}
  if(is.na(p9999)){stop("99.99% quantile is missing")}
  
  if(length(p99)>1){
    warning("More then one 99% quantile, using first value")
    p99 <- p99[1]
  }
  if(length(p995)>1){
    warning("More then one 99.5% quantile, using first value")
    p995 <- p995[1]
  }
  if(length(p999)>1){
    warning("More then one 99.9% quantile, using first value")
    p999 <- p999[1]
  }
  if(length(p9999)>1){
    warning("More then one 99.99% quantile, using first value")
    p9999 <- p9999[1]
  }
  
  #calculate starting value for optimization
  shape_start_value <- get_shape_ratio995(p99 / p995)
  
  #optimizer
  bobyqa(shape_start_value, estimate_shape, 
         p99 = p99, p995 = p995, p999 = p999, p9999 = p9999)
  
}

#' input:   shape estimate and 99&, 99.5%, 99.9% and 99.99% 
#' quantile of one year/country combination
#' output:   sum of loss from estimation and true values
estimate_shape <- function(shape, p99, p995, p999, p9999){
  loss(p99, p995, shape, 2) +
    loss(p995, p999, shape, 5) +
    loss(p99, p999, shape, 10) +
    loss(p99, p9999, shape, 100)
}


#' input:   two quantiles with the corresponding factor value and the shape 
#' estimate
#' output:  quadratic loss for two quantiles
loss <- function(p1, p2, shape, factor){
  (((p1/p2)^(1-shape)) - factor)^2
}

match_ratios(10^6,2*10^6,10^7,10^8)

##b)

#' input:   99&, 99.5%, 99.9% and 99.99% quantile of one year/country 
#' combination
#' and type of loss function to use
#' output:  estimated shape value and convergence informations
match_ratios <- function(p99, p995, p999, p9999, 
                         type=c("quadratic", "absolute", "relative")){
  
  if(!is.vector(p99)){stop("99% quantile is not a correct format")}
  if(!is.vector(p995)){stop("99.5% quantile is not a correct format")}
  if(!is.vector(p999)){stop("99.9% quantile is not a correct format")}
  if(!is.vector(p9999)){stop("99.99% quantile is not a correct format")}
  
  if(is.na(p99)){stop("99% quantile is missing")}
  if(is.na(p995)){stop("99.5% quantile is missing")}
  if(is.na(p999)){warning("99.9% quantile is missing")}
  if(is.na(p9999)){warning("99.99% quantile is missing")}
  
  if(!is.numeric(p99)){stop("99% quantile is not numeric")}
  if(!is.numeric(p995)){stop("99.5% quantile is not numeric")}
  if(!is.numeric(p999) & !is.na(p999)){stop("99.9% quantile is not numeric")}
  if(!is.numeric(p9999) & !is.na(p9999)){stop("99.99½ quantle is not numeric")}
  
  if(length(p99)>1){
    warning("More then one 99% quantile, using first value")
    p99 <- p99[1]
  }
  if(length(p995)>1){
    warning("More then one 99.5% quantile, using first value")
    p995 <- p995[1]
  }
  if(length(p999)>1){
    warning("More then one 99.9% quantile, using first value")
    p999 <- p999[1]
  }
  if(length(p9999)>1){
    warning("More then one 99.99% quantile, using first value")
    p9999 <- p9999[1]
  }
  
  require(minqa)
  
  type <- match.arg(type)
  
  shape_start_value <- get_shape_ratio995(p99 / p995)
  
  bobyqa(shape_start_value, estimate_shape, 
         p99 = p99, p995 = p995, p999 = p999, p9999 = p9999, type = type)
  
}

#' input:   shape estimate and 99&, 99.5%, 99.9% and 99.99% quantile of one 
#' year/country combination and the type of loss to use
#' output:   sum of loss from estimation and true values
estimate_shape <- function(shape, p99, p995, p999, p9999, type){
  loss(p99, p995, shape, 2, type) +
    loss(p995, p999, shape, 5, type) +
    loss(p99, p999, shape, 10, type) +
    loss(p99, p9999, shape, 100, type)
}

#' input:   two quantiles with the corresponding factor value and the shape 
#' estimate and type of loss to use
#' output:  specified loss for two quantiles
loss <- function(p1, p2, shape, factor, type){
  
  if(is.na(p1) | is.na(p2)){
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


# use true quantiles of known Pareto Type 1:
# p_vec <- qpareto_1(c(.99, .995, .999, .9999), shape=2, xmin=10000)
p_vec <- c(1e+06, 2e+06, 1e+07, 1e+08)
match_ratios(p_vec[1], p_vec[2], p_vec[3], p_vec[4], type = "quadratic")
match_ratios(p_vec[1], p_vec[2], p_vec[3], p_vec[4], type = "absolute")
match_ratios(p_vec[1], p_vec[2], p_vec[3], p_vec[4], type = "relative")


# some of these should fail (gracefully, with an informative error message!)
# -- which ones?
match_ratios(NA, NA, p_vec[3], p_vec[4])
match_ratios(NA, p_vec[2], NA, p_vec[4])
match_ratios(NA, p_vec[2], p_vec[3], NA)
match_ratios(p_vec[1], NA, NA, p_vec[4])
match_ratios(p_vec[1], p_vec[2], NA, NA)



#c)
#' input:   99&, 99.5%, 99.9% and 99.99% quantile of one year/country 
#' combination
#' output:  estimated shape value or a missing value in case of an error

match_ratios_wrapper <- function(p99, p995, p999, p9999, type){
  tryCatch(
{match_ratios(p99, p995, p999, p9999, type)$par},
error = function(e){
  return(NA)
})
}

#' input:   dataset with country year and the quantiles
#' output:  estimated shape values for each year/country
match_ratios_sequentiel <- function(data, 
                                    type = c("quadratic", 
                                             "absolute", "relative")){
  
  if(!is.data.frame(data)){stop("<data> should be a dataframe")}
  if(! "country" %in% colnames(data)){stop("No country variable found")}
  if(! "year" %in% colnames(data)){stop("No year variable found")}
  if(! "p99" %in% colnames(data)){stop("No variable for 99% quantile found")}
  if(! "p995" %in% colnames(data)){stop("No variable for 99.5% quantile found")}
  if(! "p999" %in% colnames(data)){stop("No variable for 99.9% quantile found")}
  if(! "p9999" %in% colnames(data)){
    stop("No variable for 99.99% quantile found")}
  
  type <- match.arg(type, several.ok = TRUE)
  
  processed_data <- subset(data, select=c(country, year))
  
  for(t in type){
    processed_data[, paste0(t, "ShapeApprox")] <- mapply(match_ratios_wrapper, 
                                                         p99 = data$p99, 
                                                         p995 = data$p995, 
                                                         p999 = data$p999, 
                                                         p9999 = data$p9999, 
                                                         type = t)
  }
  processed_data
}


#' input:   dataset with country year and the quantiles
#' output:  estimated shape values for each year/country
match_ratios_parallel <- function(data, cores = 3, 
                                  type = c("quadratic", 
                                           "absolute", "relative")){
  
  
  require(parallel)
  
  if(!is.data.frame(data)){stop("<data> should be a dataframe")}
  if(! "country" %in% colnames(data)){stop("No country variable found")}
  if(! "year" %in% colnames(data)){stop("No year variable found")}
  if(! "p99" %in% colnames(data)){stop("No variable for 99% quantile found")}
  if(! "p995" %in% colnames(data)){stop("No variable for 99.5% quantile found")}
  if(! "p999" %in% colnames(data)){stop("No variable for 99.9% quantile found")}
  if(! "p9999" %in% colnames(data)){
    stop("No variable for 99.99% quantile found")}
  
  type <- match.arg(type, several.ok = TRUE)
  
  if(is.null(cores)){
    cores <- detectCores() - 1
  }
  ## parallel apply-function is platform-dependent:
  if(.Platform$OS.type != "windows") {
    # simply use forking for unix, mac:
    pmapply <- function(FUN, ...) {
      mcmapply(FUN = FUN, ..., mc.cores = cores)
    }
  } else {
    # use socket clusters on windows
    if(is.null(cluster)) {
      cluster <- makePSOCKcluster(rep("localhost", detectCores()-1))
      on.exit(stopCluster(cl = cluster))
    }
    # make sure RNG is set to parallel mode:
    RNGkind("L'Ecuyer-CMRG")
    clusterSetRNGStream(cluster)
    pmapply <- function(FUN, ...) {
      clusterMap(cl = cluster, fun = FUN, ...)
    }
  }
  
  processed_data <- subset(data, select=c(country, year))
  
  for(t in type){
    processed_data[, paste0(t, "ShapeApprox")] <- mapply(match_ratios_wrapper, 
                                                         p99 = data$p99, 
                                                         p995 = data$p995, 
                                                         p999 = data$p999, 
                                                         p9999 = data$p9999, 
                                                         type = t)
  }
  processed_data
}


data <- subset(wtid_clean, select=c(country, year, p99, p995, p999, p9999), 
               year %in% c(1913:2012) & 
               country %in% c("United States", "France"))

data_seqentially_calculated <- match_ratios_sequentiel(data)
data_parallel_calculated <- match_ratios_parallel(data)

identical(data_parallel_calculated, data_seqentially_calculated)


library(ggplot2)


ggplot(data = data_parallel_calculated) +
  geom_line(aes(x = year, y = quadraticShapeApprox, color = country, 
                lty = country)) +
  ylab("shape estimate") +
  ggtitle("Shape estimates: quadratic") + 
  theme_minimal(base_size=18)

ggplot(data = data_parallel_calculated) +
  geom_line(aes(x = year, y = absoluteShapeApprox, color = country, 
                lty = country)) +
  ylab("shape estimate") +
  ggtitle("Shape estimates: absolute") + 
  theme_minimal(base_size=18)

ggplot(data = data_parallel_calculated) +
  geom_line(aes(x = year, y = relativeShapeApprox, color = country, 
                lty = country)) +
  ylab("shape estimate") +
  ggtitle("Shape estimates: relative") + 
  theme_minimal(base_size=18)
