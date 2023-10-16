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
    SS <- prod(rgvariates)
    ##
    if(missing(alphas) || (is.logical(alphas) && alphas)){
        alphas <- 2^seq(floor(-log2(SS)), 0, by=1)
    }
    ## print(alphas)
    names(rgvariates) <- variates
    freqs <- numeric(SS)
    dim(freqs) <- rgvariates
    dimnames(freqs) <- apply(metadata,1,function(xx){unname(xx[paste0('V',1:(xx['domainsize']))])}, simplify=list)
    names(dimnames(freqs)) <- variates
    if(verbose){cat('\nUpdating internal state of knowledge, please wait... ')}
    ## alphas <- array(0, dim=rgvariates,
    ##                 dimnames=apply(metadata,1,function(xx){unname(xx[paste0('V',1:(xx['domainsize']))])}, simplify=list))
    ##
    if(!(is.null(data) || all(is.na(data)))){
        ## for-loop faster than apply
        for(arow in 1:nrow(data)){
            temp <- rbind(as.character(data[arow,..variates]))
            freqs[temp] <- freqs[temp] + 1
        }
    }
    NN <- sum(freqs)
    ## if(is.null(dim(freqs))){
    ##     tempnames <- names(freqs)
    ##     dim(freqs) <- length(freqs)
    ##     dimnames(freqs) <- list(tempnames)
    ## }
    ##
    ## timethis <- Sys.time()
    ## palphas <- numeric(length(alphas))
    ## for(i in 1:length(alphas)){
    ##     palphas[i] <- sum(lgamma(freqs + alphas[i]/SS))
    ## }
    ## print(Sys.time()-timethis)
    ## timethis <- Sys.time()
    ffs <- tabulate(c(freqs)+1)
    iffs <- which(ffs > 0) - 1
    ffs <- ffs[iffs+1]
    palphas <- sapply(alphas, function(alpha){sum(ffs*lgamma(iffs + alpha))}) -
        lgamma(SS*alphas+NN) - SS*lgamma(alphas) + lgamma(SS*alphas) 
    ## print(Sys.time()-timethis)
    ## ##
    ## timethis <- Sys.time()
    ## palphas2 <- sapply(alphas, function(alpha){sum(lgamma(freqs + alpha))})
    ## print(Sys.time()-timethis)
    if(verbose){cat('Done.\n')}
    palphas <- exp(palphas-max(palphas))
    ## palphas <- palphas/sum(palphas)
    ##
    list(freqs=freqs, alphas=alphas, palphas=palphas)
}

fprobability <- function(finfo, marginal=NULL, conditional=NULL){
    variates <- names(dimnames(finfo$freqs))
    ## ##
    ## ## Sanity checks
    ## if(!is.null(conditional) && !all(names(conditional) %in% variates)){
    ##     stop('Unknown conditional variates.')
    ## }
    ## if(length(setdiff(variates, names(conditional))) < 1){
    ##     stop('All variates are in the conditional.')
    ## }
    ## if(!is.null(marginal) && !all(marginal %in% variates)){
    ##     stop('Unknown marginal variates.')
    ## }
    ## ## if(is.null(marginal)){
    ## ##     marginal <- setdiff(variates, names(conditional))
    ## ## }
    ## if(!is.null(marginal) && length(intersect(marginal, names(conditional))) > 0){
    ##     stop('Common variates in marginal and conditional.')
    ## }
    ## ##
    ## Selection of conditional values
    if(!is.null(conditional)){
        conditional <- as.list(conditional)
        iconditional <- match(names(conditional), variates)
        totake <- as.list(rep(TRUE, length(variates)))
        totake[iconditional] <- conditional
        freqs <- do.call(`[`, c(list(finfo$freqs), totake))
        if(is.null(dim(freqs))){
            dim(freqs) <- length(freqs)
            dimnames(freqs) <- dimnames(finfo$freqs)[-iconditional]
        }
    }else{
        freqs <- finfo$freqs
        iconditional <- NULL
    }
    ## Selection of marginal variates
    if(!is.null(marginal)){
        ## imarginal <- match(marginal, variates)
        freqs <- apply(freqs, marginal, sum)
        if(is.null(dim(freqs))){
            dim(freqs) <- length(freqs)
            dimnames(freqs) <- dimnames(finfo$freqs)[marginal]
        }
        alphas <- finfo$alphas*prod(dim(freqs)[-match(marginal, names(dimnames(freqs)))])
    }else{
        marginal <- setdiff(variates, names(conditional))
        imarginal <- match(marginal, variates)
        alphas <- finfo$alphas
    }
    ##
    ## NN <- sum(finfo$freqs)
    ## SS <- prod(dim(finfo$freqs)[c(imarginal,iconditional)])
    ##
    ## out <- 0L*freqs
    ## for(i in 1:length(finfo$alphas)){
    ##     temp <- freqs + finfo$alphas[i]/SS
    ##     out <- out + finfo$palphas[i] * temp/sum(temp)
    ## }
    ## out/sum(finfo$palphas)
    colSums(finfo$palphas *
            aperm(sapply(alphas, function(alpha){
                temp <- freqs + alpha
                temp/sum(temp)
            }, simplify='array'), c(length(dim(freqs))+1, 1:length(dim(freqs))))
        )/sum(finfo$palphas)
}

fprobability1D <- function(finfo, prob, log=FALSE){
    extraDistr::ddirichlet(x=prob, alpha=rbind(finfo), log=log)
}

fmarginal <- function(finfo, marginal){
    imarginal <- match(marginal, names(dimnames(finfo$freqs)))
    freqs <- apply(finfo$freqs, imarginal, sum)
    if(is.null(dim(freqs))){
        dim(freqs) <- length(freqs)
        dimnames(freqs) <- dimnames(finfo$freqs)[imarginal]
    }
    list(freqs=freqs, alphas=finfo$alphas*prod(dim(finfo$freqs)[-imarginal]), palphas=finfo$palphas)
}

fconditional <- function(finfo, conditional){
    variates <- names(dimnames(finfo$freqs))
    conditional <- as.list(conditional)
    iconditional <- match(names(conditional), variates)
    totake <- as.list(rep(TRUE, length(variates)))
    totake[iconditional] <- conditional
    freqs <- do.call('[', c(list(finfo$freqs), totake))
    ## freqs <- freqs # *sum(finfo)/sum(freqs)
    if(is.null(dim(freqs))){
        dim(freqs) <- length(freqs)
        dimnames(freqs) <- dimnames(finfo$freqs)[-iconditional]
    }
    list(freqs=freqs, alphas=finfo$alphas, palphas=finfo$palphas)
}


fpredict <- function(finfo){
    SS <- length(finfo$freqs)
    NN <- sum(finfo$freqs)
    colSums(finfo$palphas *
        aperm(sapply(finfo$alphas, function(alpha){
            out <- finfo$freqs+alpha
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
