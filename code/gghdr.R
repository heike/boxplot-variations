gghdr <- function(group, y, probs= c(90, 50, 25), data) {
  require(hdrcde)
  group <- data[,group]
  y <- data[, y]
  data <- data.frame(group, y)
  hdr.df <- ddply(data, .(group), function(x) {
    res <- hdr(x$y, prob=probs)
    m <- res$hdr
    k <- dim(m)[2]/2
    out <- data.frame(x1=m[,1], x2=m[,2])
    if (k > 1)
      for (i in 2:k) 
        out <- rbind(out, data.frame(x1=m[,2*i-1], x2=m[,2*i]))
    out$probs <- probs
    out$group <- x$group[1]
    out$mode <- res$mode
    out  
  })
  hdr.df <- na.omit(hdr.df)
  
  outliers <- ddply(data, .(group), function(x) {
    outsub <- subset(hdr.df, group==x$group[1])
    res <- x[x$y > max(outsub$x2) | x$y < min(outsub$x1),]
    res
  })
  
  
  p <- ggplot(aes(fill=factor(group)), data=hdr.df) + 
    geom_rect(aes(xmin=group-0.4, #*sqrt(probs/100),          
                  xmax=group+0.4, #*sqrt(probs/100), 
                  ymin=x1, ymax=x2), alpha=0.5) + 
    geom_segment(aes(x=group-0.45,
                     xend=group+0.45,
                     y=mode, yend=mode,
                     colour=factor(group))) + 
    geom_point(aes(x=group, y=y), data=outliers) + 
    scale_x_continuous(breaks=unique(group)) + 
    theme(legend.position="none") + xlab("") + ylab("")
  
    
  p
}

# 
#frame <- simstudy(type=3, pars=list(n=96, mu=4, n1=48, group=2))
#gghdr(y="vals", group="group", data=frame)
#ggsave(file="images/hdr-xpl.pdf", width=2, height=2)