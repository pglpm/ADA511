buildagent <- function(metadata, data=NULL, kmi=0, kma=20, alphas=NULL, base=2){
#### Builds "agent" object encoding background & learned knowledge
#### Requires 'data.table'
    ##
    ## Read metadata from file, if given as file
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
    ## Possibility of adding a p(k) weight in the Dirichlet-mixture distribution
    logpalphas0 <- 0 # do not use
    ## logpalphas0 <- -abs(alphas) # Good's hyperprior
    ##
    ## counts := #z  are the joint absolute frequencies
    ## of all possible variate values
    counts <- numeric(M) # zero vector
    dim(counts) <- domainsizes # transform to array
    ## give names to the array elements, from metadata
    dimnames(counts) <- apply(metadata, 1,
                             function(metadatum){
                                 unname(metadatum[paste0('V',1:(metadatum['domainsize']))])
                             }, simplify=list)
    names(dimnames(counts)) <- variates
    ##
    ## Calculate #z from data, if data are given
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
    ## Calculate frequencies of counts for faster iteration
    freqscounts <- tabulate(c(counts)+1)
    counts1 <- which(freqscounts > 0) # discard non-appearing ones count values
    freqscounts <- freqscounts[counts1]
    ##
    ## final auxalphas := log-probability for new unit
    auxalphas <- sapply(alphas, function(alpha){
        sum(freqscounts * lgamma(counts1-1 + alpha/M))
    })  - M*lgamma(alphas/M) + lgamma(alphas) + logpalphas0
    ##
    ## Final palphas := probability distribution for the alpha parameters
    ### used for population-frequency forecasts
    palphas <- auxalphas - lgamma(alphas + NN)
    palphas <- exp(palphas-max(palphas)) # renormalize against overflow
    palphas <- palphas/sum(palphas)
    ##
    auxalphas <- auxalphas - lgamma(alphas + NN + 1)
    ##
    ## Output "agent" object
    out <- list(counts=counts, alphas=alphas, auxalphas=auxalphas, palphas=palphas)
    class(out) <- c('agent', class(out))
    out
}
