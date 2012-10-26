
# draw final state: opinions or belief
svm.draw.final <- function(file, col=3, zlim=NA) {
  d <- read.table(file=file, header=TRUE)[-1,] # removing row types
  
  ## fix the order
  x <- sort(unique(d[[1]]));  y <- sort(unique(d[[2]]))
  d <- d[order(d[[2]], d[[1]]),]

  z <- array(data=d[[col]], dim=c(length(unique(d[[1]])), length(unique(d[[2]]))))
  if(any(is.na(zlim))) zlim <- range(z)

  persp(x, y, z,
        zlim=zlim,
        expand=0.5,
        ticktype='detailed',
        xlab=names(d)[1], ylab=names(d)[2], zlab=names(d)[col],
        r=10, phi=20, theta=40, shade=0.3)
}
