buildagent <- function(metadata, data=NULL, kmi=-17, kma=17, alphas=NULL, base=2){
#### Build object encoding background knowledge and learned knowledge
#### Requires 'data.table'
    ##
    if(is.character(metadata)){
        metadata <- fread(metadata, na.strings='', header=TRUE)
    }
    ##
    variates <- metadata[['variate']] # list of variates
    nvariates <- length(variates) # total number of variates
    domainsizes <- metadata[['domainsize']]
    names(domainsizes) <- variates
    M <- prod(domainsizes) # total number of possible values
    ##
    ## Load training data, if given as file
    if(is.character(data)){
        data <- fread(data, na.strings='', header=TRUE)
    }
    ##
    ## Building a Dirichlet-mixture distribution
    ## with concentration parameters alpha:=(2^k)
    if(is.null(alphas)){
        alphas <- base^(kmi:kma)
    }else if(is.logical(alphas) && alphas){
        alphas <- sqrt(M) # other alternative with just one k
    }
    ## logpalphas0 <- -abs(alphas) # Good's hyperprior
    logpalphas0 <- 0 # do not use
    ## print(alphas)
    ##
    ## counts := #z
    ## are the joint absolute frequencies of all possible variate values
    counts <- numeric(M) # zero vector
    dim(counts) <- domainsizes # transform to array
    ## give names to the array elements, from metadata
    dimnames(counts) <- apply(metadata, 1,
                             function(metadatum){
                                 unname(metadatum[paste0('V',1:(metadatum['domainsize']))])
                             }, simplify=list)
    names(dimnames(counts)) <- variates
    ##
    ## increase counts from data, if given
    if(!is.null(data)){
        ## Check consistency of variates in metadata and data
        if(length(setdiff(variates, colnames(data))) > 0){
            stop('Some metadata variates are missing in data')
        }
        if(length(setdiff(colnames(data), variates)) > 0){
            message('Discarding data variates not present in metadata')
        }
        data <- data[,..variates]
        ##
        ## fill absolute frequency values from data
        ## datapoints with missing values are discarded
        ## (note to self: for-loop was faster than 'apply')
        for(arow in 1:nrow(data)){
            datum <- data[arow]
            if(!any(is.na(datum))){
                temp <- rbind(as.character(datum))
                counts[temp] <- counts[temp] + 1
            }
        }
    }
    ##
    NN <- sum(counts) # total number of training datapoints
    ##
    ## Calculate probability distribution for the alpha parameters
    ## Use frequencies of counts for faster iteration
    freqscounts <- tabulate(c(counts)+1)
    counts1 <- which(freqscounts > 0) # discard non-appearing ones
    freqscounts <- freqscounts[counts1]
    auxalphas <- sapply(alphas, function(alpha){
        sum(freqscounts * lgamma(counts1-1 + alpha/M))
    })  - M*lgamma(alphas/M) + lgamma(alphas) + logpalphas0
    ##
    ## Updated probabilities of alpha parameters, for frequency forecasts
    palphas <- auxalphas - lgamma(alphas + NN)
    palphas <- exp(palphas-max(palphas))
    palphas <- palphas/sum(palphas)
    ##
    auxalphas <- auxalphas - lgamma(alphas + NN + 1)
    ##
    list(counts=counts, alphas=alphas, auxalphas=auxalphas, palphas=palphas)
}
