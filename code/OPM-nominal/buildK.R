buildK <- function(metadata, data=NULL, alphas=NULL){
#### Build object encoding background knowledge and learned knowledge
#### Requires 'data.table'
    ##
    if(is.character(metadata)){
        metadata <- fread(metadata, na.strings='', header=TRUE)
    }
    ##
    variates <- metadata[['variate']]
    nvariates <- nrow(metadata)
    rgvariates <- metadata[['domainsize']]
    names(rgvariates) <- variates
    M <- prod(rgvariates)
    ##
    if(is.character(data)){
        data <- fread(data, na.strings='', header=TRUE)
    }
    ##
    ## Building a Dirichlet-mixture distribution
    ## with concentration params alphas
    if(is.null(alphas)){
        alphas <- 2^seq(floor(-log2(M)), 2, by=1)
    }else if(is.logical(alphas) && alphas){
        alphas <- 1/sqrt(M)
    }
    ## logpalphas0 <- -abs(alphas) # Good's hyperprior
    logpalphas0 <- 0
    ## print(alphas)
    ## freqs encodes the joint frequencies of observed data
    freqs <- numeric(M) # zero vector
    dim(freqs) <- rgvariates # zero array
    dimnames(freqs) <- apply(metadata, 1,
                             function(xx){
                                 unname(xx[paste0('V',1:(xx['domainsize']))])
                             }, simplify=list)
    names(dimnames(freqs)) <- variates
    ##
    if(!is.null(data)){
        ## Check consistency of variates in metadata and data
        if(length(setdiff(variates, colnames(data))) > 0){
            stop('Some metadata variates missing in data')
        }
        if(length(setdiff(colnames(data), variates)) > 0){
            message('Discarding data variates not present in metadata')
        }
        data <- data[,..variates]
        ##
        ## fill freqs values
        ## (for-loop faster than apply)
        for(arow in 1:nrow(data)){
            datum <- data[arow]
            if(!any(is.na(datum))){
                temp <- rbind(as.character(datum))
                freqs[temp] <- freqs[temp] + 1
            }
        }
    }
    NN <- sum(freqs)
    ##
    ## Calculate probability distribution for the alpha parameters
    ## Use frequencies of frequencies for faster iteration
    ffs <- tabulate(c(freqs)+1)
    iffs <- which(ffs > 0)
    ffs <- ffs[iffs]
    valphas <- sapply(alphas, function(alpha){
        sum(ffs * lgamma(iffs-1 + alpha))
    }) + logpalphas0
    ##
    ## Updated probabilities of alpha parameters, for frequency forecasts
    palphas <- valphas - lgamma(M*alphas+NN) - M*lgamma(alphas) + lgamma(M*alphas)
    palphas <- exp(palphas-max(palphas))
    palphas <- palphas/sum(palphas)
    ##
    valphas <- valphas - lgamma(M*alphas+NN+1)
    valphas <- valphas-max(valphas)
    ##
    list(freqs=freqs, alphas=alphas, valphas=valphas, palphas=palphas)
}
