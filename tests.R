# Tests

library(ggplot2)

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

dat <- circleFun(c(0,0),2,npoints = 100)
#geom_path will do open circles, geom_polygon will do filled circles
ggplot(dat,aes(x,y)) + geom_path()
