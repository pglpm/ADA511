plotFsamples1D <- function(agent, n=100, predict=TRUE, file=NULL){
#### Plot samples of full-population freq. distributions for one variate
#### Requires 'png' to plot png
    if(length(dim(agent[['counts']])) > 1){
        stop('State of knowledge comprises more than one variate.')
    }
    samples <- rF(n=n, agent=agent)
    if(!is.null(file)){
        filext <- sub(".*\\.|.*", "", file, perl=TRUE)
        if(filext == 'pdf'){
            pdff(sub('.pdf$', '', file))
        }else{
            pngf(sub('.png$', '', file))
        }
    }
    ##
    tplot(y=t(samples), x=1:ncol(samples), type='b',
      xticks=1:ncol(samples), xlabels=dimnames(samples)[[2]],
      xlab=bquote(italic(.(names(dimnames(samples))[2]))),
      ylab='frequency',
      ylim=c(0,NA),
      lty=1, lwd=1, pch=16, col=7, alpha=0.5, cex=0.75
      )
    if(predict){
        fmean <- colMeans(samples)
        tplot(y=fmean, x=1:ncol(samples), type='b',
              lty=1, lwd=4, pch=18, col=1, alpha=0.25, cex=1, add=T
              )
    }
    if(!is.null(file)){
        dev.off()
    }
}
