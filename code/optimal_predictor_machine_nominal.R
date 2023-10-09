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

finfo <- function(data, metadata, nalpha){
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
    if(missing(nalpha) || (is.logical(nalpha) && nalpha)){
        nalpha <- max(rgvariates)
    }
    ## print(nalpha)
    names(rgvariates) <- variates
    alphas <- array(nalpha/prod(rgvariates), dim=rgvariates,
                    dimnames=apply(metadata,1,function(xx){unname(xx[paste0('V',1:(xx['domainsize']))])}, simplify=list))
    ##
    if(!(is.null(data) || all(is.na(data)))){
        ## for-loop faster than apply
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
    attr(alphas, 'domainsize') <- metadata[['domainsize']]
    alphas
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
    ncond <- match(names(unitdata), attr(finfo,'variates'))
    totake <- as.list(rep(TRUE, length(dim(finfo))))
    totake[ncond] <- unitdata
    alphas <- do.call('[', c(list(finfo), totake))
    ## alphas <- alphas # *sum(finfo)/sum(alphas)
    if(is.null(dim(alphas))){
        tempnames <- names(alphas)
        dim(alphas) <- length(alphas)
        dimnames(alphas) <- list(tempnames)
    }
    attr(alphas, 'variates') <- attr(finfo,'variates')[-ncond]
    attr(alphas, 'domainsize') <- attr(finfo,'domainsize')[-ncond]
    alphas
}


fpredict <- function(finfo){finfo/sum(finfo)}

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
