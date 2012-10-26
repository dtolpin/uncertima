wafer.colors = c('blue', 'green', 'red', 'cyan', 'magenta',
                 'orange', 'yellow', 'pink', 'light gray', 'dark gray')

# draw final opinions
wafer.draw.opinions <- function(file, col=3, zlim=NA, best=NA) {
  data <- read.table(file=file, header=TRUE)[-1,] # removing row types
  
  ## fix the order
  x <- sort(unique(data[[1]]));  y <- sort(unique(data[[2]]))
  d <- data[order(data[[2]], data[[1]]),]

  z <- array(data=d[[col]])
  if(any(is.na(zlim))) zlim <- range(z)

  plot(x=NA, y=NA, xlim=range(x), ylim=zlim, xlab=names(d)[[1]], ylab=names(d)[[col]])

  for(i in 1:length(y)) {
    lines(spline(x, z[(1:length(x))+(i-1)*length(x)]), type='l', col=wafer.colors[i])
  }

  if(all(is.na(best))) {
    best <- which.max(z)
    points(x=x[best %% length(x)], y=z[best], col=wafer.colors[best %/% length(x)+1],pch=19,cex=2)
  } else {
    b <- read.table(file=best, header=TRUE)
    for(i in 1:dim(b)[1]) {
      points(x=b$focus[i], y=d$merit[d$focus==b$focus[i] & d$color==b$color[i]],
             col=wafer.colors[b$color[i]],
             pch=19, cex=2)
    }
  }

  legend(x='bottomright',
         legend=unlist(lapply(y,as.character)),
         col=wafer.colors[y],
         lty=rep('solid',length(y)))

}

