
# draw observation and best point
log.pairs <- function () {
  pairs(list(x=c(1,2),y=c(3,1),z=c(2,1)),xlim=c(0,4),ylim=c(0,4),bg=c(2,3),pch=c(22,23))
}

log.hists <- function () {
  barplot(cbind(c(3,5,4),c(2,2,1)),beside=T,
          col=c(5,6,7), legend=c("x","y","z"), names.arg=c("observed","best"))
}

log.progress <- function () {
  n <- 10
  plot(x=1:n,y=(sin(9*(1:n)/n)+1),ylim=c(0,4),type='b',bg=2,pch=22,xlab='time',ylab='gradient')
  lines(x=1:n,y=3/(1:n),ylim=c(0,4),type='b',bg=3,pch=23)
  legend('topright',legend=c('observed','optimal'),pch=c(22,23),pt.bg=c(2,3))
}

res.contours <- function () {
  n <- 32
  ix <- 5*n/8
  iy <- 3*n/8
  x <- 4*(1:n)/n
  y <- 4*(1:n)/n
  z <- mapply(function(x,y) abs(((x-1.5)*x-(y-2.5)*y)/(1+(x-2)*(x-2)+(y-2)*(y-2))),
              x, list(y))
  izmax <- which.max(z)
  iymax <- izmax %/% n + 1
  ixmax <- izmax %% n
  
  
  layout(matrix(c(1,3,2,0),nrow=2,byrow=T), respect=T,widths=c(2,1),heights=c(2,1))
  contour (x,y,z,xlab='x',ylab='y',main='utility',
           col=c(
             '#FF0000', '#FF3333', '#FF6666', '#FF9999', '#FFAAAA',
             '#CCFFCC', '#99FF99', '#66FF66', '#33FF33', '#00FF00',
             '#00FFFF', '#33FFFF', '#66FFFF', '#99FFFF', '#CCFFFF'),
           nlevels=16,
           lwd=3)
  abline(h=y[iy], v=x[ix],lty=4)
  points(x=x[ix], y=y[iy], col='red', cex=2, pch=22, lwd=2)
  points(x=x[ixmax], y=y[iymax], col='blue', cex=2, pch=22, lwd=2)
  plot(x=x, y=z[,iy],type='l',ylim=range(z),ylab='utility',lwd=2,pch=22,bg=3)
  plot(x=z[ix,], y=y, type='l',xlim=range(z),xlab='utility',lwd=2,pch=22,bg=3)
  
}
