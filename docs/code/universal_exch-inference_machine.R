library('data.table')
library('png')
library('foreach')
source('/home/pglpm/work/tplotfunctions.R')

finfo <- function(data, metadata){
    if(missing(data)){data <- NULL}
    if(is.character(data)){
        data <- fread(data, na.strings='')
    }
    if(is.character(metadata)){
        metadata <- fread(metadata, na.strings='')
    }
    ##
    variates <- metadata$variate
    nvariates <- nrow(metadata)
    rgvariates <- metadata$N
    names(rgvariates) <- variates
    alphas <- array(1/prod(rgvariates), dim=rgvariates,
                    dimnames=apply(metadata,1,function(xx){unname(xx[paste0('V',1:(xx['N']))])}, simplify=list))
    ##
    if(!(is.null(data) || all(is.na(data)))){
        for(arow in 1:nrow(data)){
            temp <- rbind(as.character(data[arow,..variates]))
        alphas[temp] <- alphas[temp] +1
    }
    }
    if(is.null(dim(alphas))){
        tempnames <- names(alphas)
        dim(alphas) <- length(alphas)
        dimnames(alphas) <- list(tempnames)
    }
    attr(alphas, 'variates') <- metadata$variate
    attr(alphas, 'N') <- metadata$N
    alphas
}

fmarginal <- function(finfo, variates){
    whichvars <- match(variates, attr(finfo,'variates'))
    temp <- apply(finfo, whichvars,
                  sum, na.rm=T)
    if(is.null(dim(temp))){
        tempnames <- names(temp)
        dim(temp) <- length(temp)
        dimnames(temp) <- list(tempnames)
    }
    attr(temp,'variates') <- attr(finfo,'variates')[whichvars]
    attr(temp,'N') <- attr(finfo,'N')[whichvars]
    temp
}

fconditional <- function(finfo, conditional){
    conditional <- rbind(conditional)
    ncond <- match(colnames(conditional), attr(finfo,'variates'))
    totake <- as.list(rep(TRUE, length(dim(finfo))))
    totake[ncond] <- conditional
    alphas <- do.call('[', c(list(finfo), totake))
    if(is.null(dim(alphas))){
        tempnames <- names(alphas)
        dim(alphas) <- length(alphas)
        dimnames(alphas) <- list(tempnames)
    }
    attr(alphas, 'variates') <- attr(finfo,'variates')[-ncond]
    attr(alphas, 'N') <- attr(finfo,'N')[-ncond]
    alphas
}


fpredict <- function(finfo){finfo/sum(finfo)}

fsamples <- function(n, finfo){
    temp <- array(extraDistr::rdirichlet(n, alpha=c(finfo)),
                  dim=c(n, dim(finfo)),
                  dimnames=c(list(NULL), dimnames(finfo))
                  )
    attr(temp, 'variates') <- attr(finfo,'variates')
    attr(temp, 'N') <- attr(finfo,'N')
    temp
}

fquantiles <- function(p, finfo){
    alpha0 <- sum(finfo)
    temp <- apply(finfo, 1:length(dim(finfo)),
                  function(xx){qbeta(p=p, shape1=xx, shape2=alpha0-xx)})
    dimnames(temp)[[1]] <- paste0(p*100,'%')
    temp
}


plotsamples1D <- function(finfo, n=100, predict=TRUE){
    samples <- fsamples(finfo=finfo, n=n)
    ##
    tplot(y=t(samples), x=1:ncol(samples), type='b',
      xticks=1:ncol(samples), xlabels=dimnames(samples)[[2]],
      xlab=attr(samples,'variates'), ylab='probability',
      ylim=c(0,NA),
      lty=1, lwd=1, pch=16, col=7, alpha=0.5, cex=0.75
      )
    if(predict){
        fmean <- fpredict(finfo=finfo)
        tplot(y=fmean, x=1:ncol(samples), type='b',
              lty=1, lwd=3, pch=18, col=1, alpha=0.25, cex=1, add=T
              )
    }
}
