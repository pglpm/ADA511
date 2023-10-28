buildP <- function(metadata, data=NULL, kmi=-17, kma=17, alphas=NULL){
#### Build object encoding background knowledge and learned knowledge
#### Requires 'data.table'
    ##
    if(is.character(metadata)){
        metadata <- fread(metadata, na.strings='', header=TRUE)
    }
    ##
    variates <- metadata[['variate']] # list of variates
    nvariates <- nrow(metadata) # total number of variates
    domainsizes <- metadata[['domainsize']]
    names(domainsizes) <- variates
    M <- prod(domainsizes) # total number of possible values
    ##
    if(is.character(data)){
        data <- fread(data, na.strings='', header=TRUE)
    }
    ##
    ## Building a Dirichlet-mixture distribution
    ## with concentration parameters 'alphas'
    if(is.null(alphas)){
        alphas <- 2^(kmi:kma)/M
    }else if(is.logical(alphas) && alphas){
        alphas <- 1/sqrt(M)
    }
    ## logpalphas0 <- -abs(alphas) # Good's hyperprior
    logpalphas0 <- 0
    ## print(alphas)
    ## freqs encodes the joint frequencies of observed data
    freqs <- numeric(M) # zero vector
    dim(freqs) <- domainsizes # transform to array
    ## give names to the array elements
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
        ## fill frequency values from data
        ## (note to self: for-loop faster than apply)
        for(arow in 1:nrow(data)){
            datum <- data[arow]
            if(!any(is.na(datum))){
                temp <- rbind(as.character(datum))
                freqs[temp] <- freqs[temp] + 1
            }
        }
    }
    NN <- sum(freqs) # total number of training datapoints
    ##
    ## Calculate probability distribution for the alpha parameters
    ## Use frequencies of counts for faster iteration
    countsfreqs <- tabulate(c(freqs)+1)
    countsplusone <- which(countsfreqs > 0) # discard non-appearing ones
    countsfreqs <- countsfreqs[countsplusone]
    valphas <- sapply(alphas, function(alpha){
        sum(countsfreqs * lgamma(countsplusone-1 + alpha))
    })  - M*lgamma(alphas) + lgamma(M*alphas) + logpalphas0
    ##
    ## Updated probabilities of alpha parameters, for frequency forecasts
    palphas <- valphas - lgamma(M*alphas + NN)
    palphas <- exp(palphas-max(palphas))
    palphas <- palphas/sum(palphas)
    ##
    valphas <- valphas - lgamma(M*alphas + NN + 1)
    ##
    list(freqs=freqs, alphas=alphas, valphas=valphas, palphas=palphas)
}
