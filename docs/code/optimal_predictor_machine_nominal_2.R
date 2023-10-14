library('data.table')
library('png')
library('foreach')
## source('tplotfunctions.R')

guessmetadata <- function(data, file){
        if(is.character(data)){
            if(missing(file)){
                file <- paste0('meta_',data)
            }
            data <- fread(data, na.strings='')
        }else{
            if(missing(file)){
                cat("\n'file' argument missing: output to stdout\n")
                file <- NULL
            }
        }
        ##
        nvariates <- ncol(data)
        nn <- sapply(data, function(xx){length(unique(xx))})
        maxN <- max(nn)
        ## str(matrix(NA, nrow=nvariates,ncol=2+maxN, dimnames=list(NULL, c('variate','domainsize',paste0('V',1:maxN)))))
        metadata <- as.data.table(matrix(character(), nrow=nvariates,ncol=2+maxN, dimnames=list(NULL, c('variate','domainsize',paste0('V',1:maxN)))))
        ## print(metadata)
        metadata[['variate']] <- colnames(data)
        metadata[['domainsize']] <- nn
        for(i in 1:nvariates){
            metadata[i, paste0('V',1:nn[i]) := as.list(sort(unique(data[[i]])))]
        }
        ##
        if(!is.null(file)){
            fwrite(x=metadata, file=file)
        }else{
            metadata
        }
}

finfo <- function(data, metadata, alphas, verbose=TRUE){
    if(missing(data)){data <- NULL}
    if(missing(metadata)){metadata <- NULL}
    if(is.null(data) & is.null(metadata)){stop("either 'data' or 'metadata' argument must be given.")}
    ##
    if(is.character(data)){
        data <- fread(data, na.strings='')
    }
    if(is.character(metadata)){
        metadata <- fread(metadata, na.strings='')
    }
    ##
    variates <- metadata$variate
    nvariates <- nrow(metadata)
    rgvariates <- metadata[['domainsize']]
    totlength <- prod(rgvariates)
    ##
    if(missing(alphas) || (is.logical(alphas) && alphas)){
        alphas <- 4^seq(-1, round(log(totlength,4))+1, by=1)
    }
    ## print(alphas)
    names(rgvariates) <- variates
    freqs <- numeric(totlength)
    dim(freqs) <- rgvariates
    dimnames(freqs) <- apply(metadata,1,function(xx){unname(xx[paste0('V',1:(xx['domainsize']))])}, simplify=list)
    if(verbose){cat('\nUpdating internal state of knowledge, please wait... ')}
    ## alphas <- array(0, dim=rgvariates,
    ##                 dimnames=apply(metadata,1,function(xx){unname(xx[paste0('V',1:(xx['domainsize']))])}, simplify=list))
    ##
    if(!(is.null(data) || all(is.na(data)))){
        ## for-loop faster than apply
        for(arow in 1:nrow(data)){
            temp <- rbind(as.character(data[arow,..variates]))
            freqs[temp] <- freqs[temp] +1
        }
    }
    if(is.null(dim(freqs))){
        tempnames <- names(freqs)
        dim(freqs) <- length(freqs)
        dimnames(freqs) <- list(tempnames)
    }
    ##
    ## for-loop is faster
    ## timethis <- Sys.time()
    palphas <- numeric(length(alphas))
    for(i in 1:length(alphas)){
        palphas[i] <- sum(lgamma(alphas[i]/totlength+freqs))- lgamma(alphas[i]+sum(freqs))
    }
    ## print(Sys.time()-timethis)
    ## timethis <- Sys.time()
    ## palphas <- sapply(alphas, function(alpha){
    ##     sum(lgamma(alpha/totlength+freqs)) - lgamma(alpha+sum(freqs))
    ## })
    ## print(Sys.time()-timethis)
    if(verbose){cat('Done.\n')}
    palphas <- palphas - totlength*lgamma(alphas/totlength) + lgamma(alphas) 
    palphas <- exp(palphas-max(palphas))
    palphas <- palphas/sum(palphas)
    ##
    list(freqs=freqs, alphas=alphas, palphas=palphas, variates=metadata[['variate']], domainsize=metadata[['domainsize']])
}

fprobability1D <- function(finfo, prob, log=FALSE){
    extraDistr::ddirichlet(x=prob, alpha=rbind(finfo), log=log)
}

fmarginal <- function(finfo, variates){
    whichvars <- match(variates, attr(finfo,'variates'))
    temp <- apply(finfo, whichvars, sum, na.rm=T)
    if(is.null(dim(temp))){
        tempnames <- names(temp)
        dim(temp) <- length(temp)
        dimnames(temp) <- list(tempnames)
    }
    attr(temp,'variates') <- attr(finfo,'variates')[whichvars]
    attr(temp,'domainsize') <- attr(finfo,'domainsize')[whichvars]
    temp
}

fconditional <- function(finfo, unitdata){
    unitdata <- as.list(unitdata)
    ncond <- match(names(unitdata), finfo$variates)
    totake <- as.list(rep(TRUE, length(dim(finfo$freqs))))
    totake[ncond] <- unitdata
    freqs <- do.call('[', c(list(finfo$freqs), totake))
    ## freqs <- freqs # *sum(finfo)/sum(freqs)
    if(is.null(dim(freqs))){
        tempnames <- names(freqs)
        dim(freqs) <- length(freqs)
        dimnames(freqs) <- list(tempnames)
    }
    list(freqs=freqs, alphas=finfo$alphas, palphas=finfo$palphas, variates=finfo$variates[-ncond], domainsize=finfo$domainsize[-ncond])
}


fpredict <- function(finfo){
    totlength <- length(finfo$freqs)
    colSums(finfo$palphas *
        aperm(sapply(finfo$alphas, function(alpha){
            out <- (finfo$freqs+alpha/totlength)/(totlength+alpha)
            out/sum(out)
        }, simplify='array'), c(length(dim(finfo$freqs))+1, 1:length(dim(finfo$freqs))))
        )
}

fsamples <- function(n, finfo){
    temp <- array(extraDistr::rdirichlet(n, alpha=c(finfo)),
                  dim=c(n, dim(finfo)),
                  dimnames=c(list(NULL), dimnames(finfo))
                  )
    attr(temp, 'variates') <- attr(finfo,'variates')
    attr(temp, 'domainsize') <- attr(finfo,'domainsize')
    temp
}

unitsamples <- function(n, finfo){
    temp <- array(extraDistr::rdirmnom(n, alpha=c(finfo)),
                  dim=c(n, dim(finfo)),
                  dimnames=c(list(NULL), dimnames(finfo))
                  )
    attr(temp, 'variates') <- attr(finfo,'variates')
    attr(temp, 'domainsize') <- attr(finfo,'domainsize')
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
      xlab=attr(samples,'variates'), ylab='frequency',
      ylim=c(0,NA),
      lty=1, lwd=1, pch=16, col=7, alpha=0.5, cex=0.75
      )
    if(predict){
        fmean <- fpredict(finfo=finfo)
        tplot(y=fmean, x=1:ncol(samples), type='b',
              lty=1, lwd=4, pch=18, col=1, alpha=0.25, cex=1, add=T
              )
    }
}
