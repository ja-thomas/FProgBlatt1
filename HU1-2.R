# Fortgeschrittene Programmierung WS 2014/15
# Hausübung 1 (due to 23.11.2014)
# Janek Thomas, Philipp J. Rösch

### 2 a)

#' Pareto density
#' 
#' inputs:  x:      vector of quantiles
#'          shape:  distribution parameter
#'          xmin:   minimum value of x
#' output:  gives the density of the Pareto distribution

dpareto_1 <- function(x, shape, xmin) {
  # input validation 
  if (is.numeric(x) == FALSE) stop("<x> must be numeric")
  if (is.numeric(shape) == FALSE) stop("<shape> must be numeric")
  if (is.numeric(xmin) == FALSE) stop("<xmin> must be numeric")
  if (xmin <= 0) stop("<xmin> must be larger zero")
  if (shape <= 1) stop("<shape> must be larger one")
  if (any(x < xmin) & length(x) == 1)
    {warning("<x> is not larger than <xmin>")}
  if (any(x < xmin) & length(x) > 1 )
    {warning("not all <x> are larger than <xmin>")}

  
  # uses dpareto_1_support() 
  sapply(x, dpareto_1_support, shape = shape, xmin = xmin)
}

# support function applying Pareto density formula
dpareto_1_support <- function(x, shape, xmin){ 
  if (x <= xmin){
    0
  } 
  else {
   (shape - 1) / xmin * (x / xmin)^(-shape)
  }
}
  
#' Pareto distribution function
#' 
#' inputs:  p:      vector of quantiles
#'          shape:  distribution parameter
#'          xmin:   minimum value of p
#' output:  gives the distribution function of the Pareto distribution
 
ppareto_1 <- function(p, shape, xmin){
  if (is.numeric(p) == FALSE) stop("<p> must be numeric")
  if (is.numeric(shape) == FALSE) stop("<shape> must be numeric")
  if (is.numeric(xmin) == FALSE) stop("<xmin> must be numeric")
  if (xmin <= 0) stop("<xmin> must be larger zero")
  if (shape <= 1) stop("<shape> must be larger one")
  if (any(p < xmin) & length(p) == 1)
    {warning("<p> is not larger than <xmin>")}
  if (any(p < xmin) & length(p) > 1 )
    {warning("not all <p> are larger than <xmin>")}

  sapply(p, ppareto_1_support, shape = shape, xmin = xmin)
  
}

# support function applying Pareto distribution function formula
ppareto_1_support <- function(p, shape, xmin){
  if (p <= xmin){
    0
  } 
  else {
    1 - (p / xmin)^(-shape + 1)
  }
}
 
### 2 b)

# set graphic parameter: two plots side by side
par(mfrow = c(1,2))

# set x values 
xvalues <- seq(from = 0, to = 5, by = 0.01)

# plots Pareto density 
# using dpareto_1() and graphic features
plot(xvalues, dpareto_1(xvalues, shape = 2, xmin = 1), col = "green", lwd = 2,
     type = "l", cex.lab = 1.5, cex.axis = 1.5, xlim = c(0,5), ylim = c(0,3),
     ylab = "f(x; shape, xmin=1)", frame.plot = FALSE)

lines(xvalues, dpareto_1(xvalues, shape = 3, xmin = 1), col = "blue", lwd = 2)
lines(xvalues, dpareto_1(xvalues, shape = 4, xmin = 1), col = "red", lwd = 2)
lines(xvalues, dpareto_1(xvalues, shape = 1e15, xmin = 1), 
      col = "black", lwd = 2)

legend("topright", 
       c(expression(shape%->%infinity), "shape=4", "shape=3", "shape=2"),
       col = c("black","red","blue","green"), lty = c(1, 1, 1), 
       lwd = c(2, 2, 2), cex = 0.8)

# plots Pareto distribution function 
# using ppareto_1() and graphic features
plot(xvalues, ppareto_1(xvalues, shape = 2, xmin = 1), col = "green", lwd = 2,
     type = "l", cex.lab = 1.5, cex.axis = 1.5, xlim = c(0,5), ylim = c(0,1),
     ylab = "f(x; shape, xmin=1)", frame.plot = FALSE)

lines(xvalues, ppareto_1(xvalues, shape = 3, xmin = 1), col = "blue", lwd = 2)
lines(xvalues, ppareto_1(xvalues, shape = 4, xmin = 1), col = "red", lwd = 2)
lines(xvalues, ppareto_1(xvalues, shape = 1e15, xmin = 1), col = "black", lwd = 2)