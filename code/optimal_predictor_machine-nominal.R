library('data.table')
library('png')
library('foreach')
## source('tplotfunctions.R')

guessmetadata <- function(data, file){
#### Guess metadata information from dataset and save it in a metadata file
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
    metadata <- as.data.table(matrix(character(),
                                     nrow=nvariates, ncol=2+maxN,
                                     dimnames=list(NULL,
                                                   c('variate', 'domainsize', paste0('V',1:maxN)))
                                     ))
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

buildK <- function(data, metadata, alphas, verbose=FALSE){
#### Build object encoding background knowledge and learned knowledge
    ## Sanity checks
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
    variates <- metadata[['variate']]
    nvariates <- nrow(metadata)
    rgvariates <- metadata[['domainsize']]
    names(rgvariates) <- variates
    SS <- prod(rgvariates)
    ##
    ## Building a discrete mixture of Dirichlet distributions
    ## with concentration params alphas
    if(missing(alphas) || is.null(alphas)){
        alphas <- 2^seq(floor(-log2(SS)), 0, by=1)
    }else if(is.logical(alphas) && alphas){
        alphas <- 1/sqrt(SS)
    }
    ## print(alphas)
    ## freqs encodes the joint frequencies of observed data
    freqs <- numeric(SS)
    dim(freqs) <- rgvariates
    dimnames(freqs) <- apply(metadata, 1,
                             function(xx){
                                 unname(xx[paste0('V',1:(xx['domainsize']))])
                             }, simplify=list)
    names(dimnames(freqs)) <- variates
    ##
    if(verbose){cat('\nUpdating internal state of knowledge, please wait... ')}
    ##
    if(!is.null(data)){
        ## fill freqs values
        ## (for-loop faster than apply)
        for(arow in 1:nrow(data)){
            datum <- data[arow, ..variates]
            if(!any(is.na(datum))){
                temp <- rbind(as.character(datum))
                freqs[temp] <- freqs[temp] + 1
            }
        }
    }
    NN <- sum(freqs)
    ##
    ## Calculate probability distribution for the alpha parameters
    ## frequencies of frequencies for faster iteration
    ffs <- tabulate(c(freqs)+1)
    iffs <- which(ffs > 0)
    ffs <- ffs[iffs]
    palphas <- sapply(alphas, function(alpha){
        sum(ffs * lgamma(iffs-1 + alpha))
    }) - lgamma(SS*alphas+NN) - SS*lgamma(alphas) + lgamma(SS*alphas)
    ##
    palphas <- exp(palphas-max(palphas))
    palphas <- palphas/sum(palphas)
    ##
    if(verbose){cat('Done.\n')}
    ##
    list(freqs=freqs, alphas=alphas, palphas=palphas)
}

fprobability <- function(K, marginal=NULL, conditional=NULL, Kout=FALSE){
#### Calculate marginal probability distribution for some variates
#### given conditional values of other variates
    variates <- names(dimnames(K[['freqs']]))
    ##
    ## Sanity checks
    if(!is.null(conditional) && !all(names(conditional) %in% variates)){
        stop('Unknown conditional variates.')
    }
    if(length(setdiff(variates, names(conditional))) < 1){
        stop('All variates are in the conditional.')
    }
    if(!is.null(marginal) && !all(marginal %in% variates)){
        stop('Unknown marginal variates.')
    }
    if(!is.null(marginal) && length(intersect(marginal, names(conditional))) > 0){
        stop('Common variates in marginal and conditional.')
    }
    ##
    ## Selection of conditional values
    if(!is.null(conditional)){
        conditional <- as.list(conditional)
        iconditional <- match(names(conditional), variates)
        totake <- as.list(rep(TRUE, length(variates)))
        totake[iconditional] <- conditional
        freqs <- do.call(`[`, c(list(K[['freqs']]), totake))
        if(is.null(dim(freqs))){
            dim(freqs) <- length(freqs)
            dimnames(freqs) <- dimnames(K[['freqs']])[-iconditional]
        }
    }else{
        freqs <- K[['freqs']]
    }
    ##
    ## Selection of marginal variates
    if(!is.null(marginal)){
        freqs <- apply(freqs, marginal, sum)
        if(is.null(dim(freqs))){
            dim(freqs) <- length(freqs)
            dimnames(freqs) <- dimnames(K[['freqs']])[marginal]
        }
        ## the concentration parameters are summed over the marginalized variates
        alphas <- K[['alphas']] *
            prod(dim(freqs)[-match(marginal, names(dimnames(freqs)))])
    }else{
        marginal <- setdiff(variates, names(conditional))
        alphas <- K[['alphas']]
    }
    ##
    if(Kout){
        ## Output an object encoding current knowledge
        list(freqs=freqs, alphas=alphas, palphas=K[['palphas']])
    }else{
        ## Output a probability distribution for the marginal variates
        ## Average over concentration parameters
        colSums( K[['palphas']] *
                 aperm(sapply(alphas, function(alpha){
                     temp <- freqs + alpha
                     temp/sum(temp)
                 }, simplify='array'),
                 c(length(dim(freqs))+1, 1:length(dim(freqs))))
                )
    }
}

rF <- function(n=1, K){
#### Return a sample of full-population frequency
    alphasample <- sample(rep(K[['alphas']],2), size=n, replace=T,
                          prob=rep(K[['palphas']],2))
    ff <- extraDistr::rdirichlet(n, alpha=outer(alphasample, c(K[['freqs']]), `+`))
    dim(ff) <- c(n,dim(K[['freqs']]))
    dimnames(ff) <- c(list(sample=NULL), dimnames(K[['freqs']]))
    ff
}

plotsamples1D <- function(K, n=100, predict=TRUE){
#### Plot samples of full-population freq. distributions for one variate
    if(length(dim(K[['freqs']])) > 1){
        stop('State of knowledge comprises more than one variate.')
    }
    samples <- rF(n=n, K=K)
    ##
    tplot(y=t(samples), x=1:ncol(samples), type='b',
      xticks=1:ncol(samples), xlabels=dimnames(samples)[[2]],
      xlab=attr(samples,'variates'), ylab='frequency',
      ylim=c(0,NA),
      lty=1, lwd=1, pch=16, col=7, alpha=0.5, cex=0.75
      )
    if(predict){
        fmean <- fprobability(K=K, Kout=F)
        tplot(y=fmean, x=1:ncol(samples), type='b',
              lty=1, lwd=4, pch=18, col=1, alpha=0.25, cex=1, add=T
              )
    }
}
