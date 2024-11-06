plotFsamples1D <- function(
    agent,
    n = 100,
    predictand = NULL,
    predictor = NULL,
    probability = TRUE,
    file = NULL,
    ...
){
#### Plots samples of full-population freq. distributions for one variate
#### Requires 'png' to plot png
    if(
    (!is.null(predictand) && length(predictand) > 1) ||
    (!is.null(predictor) && length(predictor)-length(dim(agent[['counts']])) > 1)
    ){
        stop('State of knowledge comprises more than one variate.')
    }
    samples <- rF(n=n, agent=agent, predictand=predictand, predictor=predictor)
    if(!is.null(file)){
        filext <- sub(".*\\.|.*", "", file, perl=TRUE)
        if(filext == 'pdf'){
            mypdf(sub('.pdf$', '', file))
        }else{
            mypng(sub('.png$', '', file))
        }
    }
    ##
    mytplot(y=t(samples), x=1:ncol(samples), type='b',
      xticks=1:ncol(samples), xlabels=dimnames(samples)[[2]],
      xlab=bquote(italic(.(names(dimnames(samples))[2]))),
      ylab='probability',
      lty=1, lwd=1, pch=16, col=7, alpha=0.25, cex=0.75, ...
      )
    if(probability){
        fmean <- infer(agent=agent, predictand=predictand, predictor=predictor)
        mytplot(y=fmean, x=1:ncol(samples), type='b',
              lty=1, lwd=4, pch=18, col=1, alpha=0.75, cex=1, add = TRUE
              )
    }
    if(!is.null(file)){
        dev.off()
    }
}
