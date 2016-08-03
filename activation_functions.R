library("ggplot2")

x <- seq(-3, 3, length.out = 501)

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
a <- ggplot(zz)+geom_point(aes(y=zz$sigmoid, x=zz$point), color="red")

#relu plot
a <- a+geom_point(aes(y=zz$relu, x=zz$point), color="blue")

tanh <- function(x){
  y = (exp(2*x)-1)/(exp(2*x)+1)
  return(data.frame(tanh=y, point=x))
}

zz <- merge(zz,tanh(x), by="point")

#tanh plot
a <- a+ geom_point(aes(y=zz$tanh, x=zz$point), color="black")

sigmoid2 <- function(x){
  y = 1/(1+exp(-2*x))
  return(data.frame(sigmoid2=2*y, point=x))
}

zz <- merge(zz,sigmoid2(x), by="point")

#sigmoid2 plot
a <- a+ geom_point(aes(y=zz$sigmoid2, x=zz$point), color="cyan")
a

#exponential decay plot
dates <- c(1:365)
decay <- -.007
decayed_value <- exp(dates*decay)
data_frame = data.frame(x=dates, y=decayed_value)
ggplot(data_frame, aes(x=x, y=y))+scale_y_continuous(breaks = seq(0,1, by=0.05))+geom_point()
