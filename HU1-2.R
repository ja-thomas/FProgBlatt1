### Hü1 - 2

# shape = a
# Ihre Funktionen sollten
# - die übergebenen Parameter shape und xmin überprüfen,
# - vektorisiert sein (also: einen Vektor x akzeptieren, nicht nur ein einzelnes x),
# - korrekte Werte für x außerhalb des Trägers der Verteilung zurückliefern.

# Pareto-Dichte
# f(x) = (a-1)/x_min * (x/x_min)^(-a)
# Dichte Pareto-Vert.
dpareto_1 <- function(x, shape, xmin) {
  if (is.numeric(x)==FALSE){stop("x must be numeric")}
  if (is.numeric(shape)==FALSE){stop("shape must be numeric")}
  if (is.numeric(xmin)==FALSE){stop("xmin must be numeric")}
  
  if (any(x<xmin) & length(x)==1){warning("x is not larger than xmin.")}
  if (any(x<xmin) & length(x)>1 ){warning("not all x are larger than xmin.")}
  if (xmin<=0){stop("xmin must be larger zero")}
  if (shape<=1){stop("shape must be larger one")}
  
  sapply(x, dpareto_1_support, shape=shape, xmin=xmin)
}

dpareto_1_support <- function(x, shape, xmin){
  if (x<=xmin){
    0
  } 
  else {
   (shape - 1) / xmin * (x / xmin)^(-shape)
  }
}
  

ppareto_1 <- function(p, shape, xmin){
  if (is.numeric(p)==FALSE){stop("p must be numeric")}
  if (is.numeric(shape)==FALSE){stop("shape must be numeric")}
  if (is.numeric(xmin)==FALSE){stop("xmin must be numeric")}
  
  if (any(p<xmin) & length(p)==1){warning("p is not larger than xmin.")}
  if (any(p<xmin) & length(p)>1 ){warning("not all p are larger than xmin.")}
  if (xmin<=0){stop("xmin must be larger zero")}
  if (shape<=1){stop("shape must be larger one")}
  
  sapply(p, ppareto_1_support, shape=shape, xmin=xmin)
  
}

ppareto_1_support <- function(p, shape, xmin){
  if (p<=xmin){
    0
  } 
  else {
    1 - (p / xmin)^(-shape + 1)
  }
}
 

par(mfrow=c(1,2))

xvalues=seq(from=0, to=5, by=0.01)
plot(xvalues, dpareto_1(xvalues, shape=2, xmin=1), col="green", lwd=2, type="l", 
     cex.lab=1.5, cex.axis=1.5, xlim=c(0,5), ylim=c(0,3),frame.plot=FALSE, ylab="f(x; shape, xmin=1)")
lines(xvalues, dpareto_1(xvalues, shape=3, xmin=1), col="blue", lwd=2)
lines(xvalues, dpareto_1(xvalues, shape=4, xmin=1), col="red", lwd=2)
lines(xvalues, dpareto_1(xvalues, shape=1e15, xmin=1), col="black", lwd=2)
legend("topright", c(expression(shape%->%infinity),"shape=4","shape=3","shape=2"),
       col=c("black","red","blue","green"), lty=c(1,1,1), lwd=c(2,2,2),
       cex=0.8)


xvalues=seq(from=0, to=5, by=0.01)
plot(xvalues, ppareto_1(xvalues, shape=2, xmin=1), col="green", lwd=2, type="l", 
     cex.lab=1.5, cex.axis=1.5, xlim=c(0,5), ylim=c(0,1),frame.plot=FALSE, ylab="f(x; shape, xmin=1)")
lines(xvalues, ppareto_1(xvalues, shape=3, xmin=1), col="blue", lwd=2)
lines(xvalues, ppareto_1(xvalues, shape=4, xmin=1), col="red", lwd=2)
lines(xvalues, ppareto_1(xvalues, shape=1e15, xmin=1), col="black", lwd=2)




# Problem: grüne Linie wird nicht richtig angezeigt, aber ich denke auch 
# irgendwie, dass sie falsch ist. x darf ja nicht kleiner als xmin sein?!