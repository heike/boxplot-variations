### extract the values for plotting kernel density according to package vioplot
### copied (except for return statement) verbatim from function vioplot, package vioplot v0.2
vioplot.kernel <- function(x, at, h = NA, range = 1.5, wex = 1){
  require(sm)
  datas <- list(x)
  n <- length(datas)
  if (missing(at)) {
    at <- 1:n
  }
  upper <- vector(mode = "numeric", length = n)
  lower <- vector(mode = "numeric", length = n)
  q1 <- vector(mode = "numeric", length = n)
  q3 <- vector(mode = "numeric", length = n)
  med <- vector(mode = "numeric", length = n)
  base <- vector(mode = "list", length = n)
  height <- vector(mode = "list", length = n)
  baserange <- c(Inf, -Inf)
  args <- list(display = "none")
  if (!(is.na(h))){ 
    args <- c(args, h = h)
  }
  for (i in 1:n) {
    data <- datas[[i]]
    data.min <- min(data)
    data.max <- max(data)
    q1[i] <- quantile(data, 0.25)
    q3[i] <- quantile(data, 0.75)
    med[i] <- median(data)
    iqd <- q3[i] - q1[i]
    
    upper[i] <- min(q3[i] + range * iqd, data.max)
    lower[i] <- max(q1[i] - range * iqd, data.min)
    est.xlim <- c(min(lower[i], data.min), max(upper[i], 
                                               data.max))
    smout <- do.call("sm.density", c(list(data, xlim = est.xlim), 
                                     args))
    hscale <- 0.4/max(smout$estimate) * wex
    base[[i]] <- smout$eval.points
    height[[i]] <- smout$estimate * hscale
    t <- range(base[[i]])
    baserange[1] <- min(baserange[1], t[1])
    baserange[2] <- max(baserange[2], t[2])
  }
  i <- 1
  return(data.frame(x = c(base[[i]], rev(base[[i]])), y = c(at[i] - height[[i]], 
                                                            rev(at[i] + height[[i]]))))
}

ggviolin <- function(group, y, data, bandwidth) {
  group <- data[, group]
  y <- data[,y]
  data <- data.frame(group, y)
  vio <- ddply(data, .(group), function(x) {
    vio <- vioplot.kernel(x$y, h = bandwidth)
    data.frame( x = vio$y + x$group[1]-1, y = vio$x,
                group = x$group[1])
  })
  
  p5 <- ggplot(vio, aes(x = x, y = y)) + 
    geom_polygon(aes(group = group, fill=factor(group)), alpha=1.25*I(0.5)) + 
    xlab("") + ylab("") + theme(legend.position="none") +
    geom_boxplot(aes(x = group, y=y, 
      group = group), colour="grey60", width = .1, data=data) +
    scale_x_continuous(breaks=unique(group))
  
  p5
}

