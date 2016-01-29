vase <- function(x, ..., names = NULL, bw = NULL, plot = TRUE) {
  
  
  all.x <- c(x, list(...))

  centers <- seq_along(all.x)
  n <- length(all.x)
  if (is.null(names)) {
    names <- names(all.x)
  }

  
  if (plot) {
    xmin <- 0.5
    xmax <- length(all.x) + 0.5

    ymin <- min(unlist(all.x), na.rm = TRUE)
    ymax <- max(unlist(all.x), na.rm = TRUE)
    
    plot(c(xmin, xmax), c(ymin, ymax), type = "n", main = "", 
          xlab = "", ylab = "", xaxt = "n", yaxt = "n")

  }
  vase <- list()
  for(i in 1:n){

    lower <- quantile(all.x[[i]], probs = .25)
    upper <- quantile(all.x[[i]], probs = .75)
    #Uex <- quantile(all.x[[i]], probs = .95)
    #Lex <- quantile(all.x[[i]], probs = 0.05)
    Hspread <- (upper - lower)[[1]]
    step <- 1.5*Hspread[[1]]
	median <- median(all.x[[i]])
	###tukey definition of whiskers
	TUex <- max(all.x[[i]][all.x[[i]] <= upper+step])
	TLex <- min(all.x[[i]][all.x[[i]] >= lower-step])

    ds <- density(all.x[[i]], bw = bw[i])
    Xs <- ds$x
    Ys <- ds$y

    in_box <- Xs < upper & Xs > lower
    Xs <- c(lower, Xs[in_box], upper)
    Ys <- c(0, Ys[in_box], 0)
    
    # Scale to 0.4
    Ys <- Ys / max(Ys) * 0.4

	mpos <- which.min(abs(Xs - median))[1]
    outliers <- (all.x[[i]] > upper+step) |	
	     			(all.x[[i]] < lower-step)

	if (plot) {	 
	    if (sum(outliers) > 0) {
	      segments(centers[i], upper+step, centers[i], upper, col = "grey")
	      segments(centers[i], lower-step, centers[i], lower, col = "grey")
	
	      points(rep(centers[i], sum(outliers)), all.x[[i]][outliers], cex = 0.5)
	    	
	    }
	  
		###body
	    lines(centers[i]+Ys, Xs)
	    lines(centers[i]-Ys, Xs)
	
		###whiskers with Tukey definition
	    segments(centers[[i]], TLex, centers[[i]], lower)
	    segments(centers[[i]], TUex, centers[[i]], upper)
	    #segments(centers[[i]], Lex, centers[[i]], lower)
	    #segments(centers[[i]], Uex, centers[[i]], upper)
	
	    ###plot a line for the median
	    segments((centers[i]-Ys)[mpos], Xs[mpos], (centers[i]+Ys)[mpos], Xs[mpos], 
	      col = gray(0.25))
	}  
	attr(Xs,"names") <- NULL
	vase[[i]] <- list(body=cbind(x=c(centers[i]+Ys, rev(centers[i]-Ys)), 
		                         y=c(Xs, rev(Xs))),
		             median=cbind(x=(centers[i]-Ys)[mpos], 
		             			  y=Xs[mpos], 
		             			  xend=(centers[i]+Ys)[mpos], 
		             			  yend=Xs[mpos]),
	                whisker=cbind(x=rep(centers[i], 2),
	                              xend=rep(centers[i], 2), 
	                              y=c(lower, upper),
	                              yend=c(TLex, TUex)), 
		            outliers=cbind(x=rep(centers[i], sum(outliers)), 
					          y=all.x[[i]][outliers])
					)
  }

  if (plot) {
  	axis(1, at = centers, labels = names)
	axis(2)
  	print(centers)
  }
  invisible(vase)
}


ggvase <- function(group, y, data, bandwidth=NULL) {
  require(plyr)
  group <- data[,group]
  y <- data[,y]
  data <- data.frame(group, y)
  vd <- dlply(data, .(group), function(x) {
    vd <- vase(x=list(x$y), bw = bandwidth, plot=FALSE)
    df <- data.frame(vd[[1]]$body)
    df$group <- x$group[1]
    df$x <- df$x + df$group[1] - 1
    df$order <- 1:nrow(df)
    body <- df
    
    df <- data.frame(vd[[1]]$median)
    df$group <- x$group[1]
    df$xend <- df$xend + df$group - 1
    df$x <- df$x + df$group - 1
    median <- df
    
    df <- data.frame(vd[[1]]$whisker)
    df$group <- x$group[1]
    df$x <- df$x + df$group - 1
    df$xend <- df$xend + df$group - 1
    whisker <- df
    
    df <- data.frame(vd[[1]]$outlier)
    if (nrow(df) > 0) {
      df$group <- x$group[1]
      df$x <- df$x + df$group - 1
    }
    outlier <- df
    
    
    return(list(body=body, median=median, whisker=whisker, outlier=outlier))
  })
  
  vdbody <- ldply(1:length(vd), function(x) {
    df <- data.frame(vd[[x]]$body)
    df$order <- 1:nrow(df)
    df
  })
  
  vdoutlier <- ldply(1:length(vd), function(x) 
    data.frame(vd[[x]]$outlier)
                     )
  
  vdmedian <- ldply(1:length(vd), function(x) 
    data.frame(vd[[x]]$median)
                    )
  
  vdwhisker <- ldply(1:length(vd), function(x) 
    data.frame(vd[[x]]$whisker)
                     )
  
  p <- ggplot(aes(x, y, group=group, fill=factor(group), colour=factor(group)),  data=vdbody) + 
    geom_polygon(alpha=0.5) + 
    geom_segment(aes(xend=xend,yend=yend), data=vdmedian) +
    geom_segment(aes(xend=xend,yend=yend), data=vdwhisker) + 
    scale_x_continuous(breaks=unique(group)) + 
    xlab("") + ylab("") + 
    theme(legend.position="none")
  
  if (!is.null(vdoutlier))	
    p <- p + geom_point(colour="grey10", size=1.5, data=vdoutlier)
  
  p
}


