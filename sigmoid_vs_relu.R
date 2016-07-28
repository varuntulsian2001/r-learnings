library("ggplot2")

x <- seq(-6, 6, length.out = 501)

sigmoid <- function(x){
  y = exp(-x)
  return(data.frame(sigmoid=1/(1+y), point=x))
}

zz <- sigmoid(x)
relu <- function(x){
  y = c(length(x))
  y[x<0]=0
  y[x>=0]=x[x>=0]
  return(data.frame(relu=y, point=x))
}

zz <- merge(zz,relu(x), by="point")

#sigmoid plot
a <- ggplot(zz)+geom_point(aes(x=zz$sigmoid, y=zz$point), color="sigmoid")

#relu plot
a <- ggplot(zz, aes(x=zz$sigmoid, y=zz$point))+geom_point()
a