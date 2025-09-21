guessmetadata <- function(
    data,
    file = NULL
){
#### Guess metadata information from dataset and save it in a metadata file
    if(is.character(data)){
        if(is.null(file)){
            file <- paste0('meta_', data)
        }
        data <- read.csv(data,
            na.strings = '', stringsAsFactors = FALSE, tryLogical = FALSE)
    }else{
        if(is.null(file)){
            cat("\n'file' argument missing: output to stdout\n")
        }
    }
    ##
    nvariates <- ncol(data)
    nn <- sapply(data, function(xx){length(unique(xx))})
    maxN <- max(nn)
    ## str(matrix(NA, nrow=nvariates,ncol=2+maxN, dimnames=list(NULL, c('variate','domainsize',paste0('V',1:maxN)))))
    metadata <- as.data.frame(matrix(character(),
        nrow = nvariates, ncol = 2 + maxN,
        dimnames = list(NULL,
            c('variate', 'domainsize', paste0('V', 1:maxN)))
    ))
    ## print(metadata)
    metadata[['variate']] <- colnames(data)
    metadata[['domainsize']] <- nn
    for(i in 1:nvariates){
        metadata[i, paste0('V', 1:nn[i])] <- as.list(sort(unique(data[[i]])))
    }
    ##
    if(!is.null(file)){
        write.csv(x = metadata, file = file,
            row.names = FALSE, quote = FALSE, na = '')
    }else{
        metadata
    }
}


buildagent <- function(
    metadata,
    data = NULL,
    kmi = 0,
    kma = 20,
    alphas = NULL,
    base = 2
){
#### Builds "agent" object encoding background & learned knowledge
    ##
    ## Read metadata from file, if given as file
    if(is.character(metadata)){
        metadata <- read.csv(metadata,
            na.strings='', stringsAsFactors = FALSE, tryLogical = FALSE)
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
        data <- read.csv(data,
            na.strings='', stringsAsFactors = FALSE, tryLogical = FALSE)
    }
    ##
    ## Building a Dirichlet-mixture distribution
    ## with concentration parameters alpha:=(2^k)
    if(is.null(alphas)){
        alphas <- base^(kmi:kma)
    }else if(isTRUE(alphas)){
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
        data <- data[, variates, drop = FALSE]
        ##
        ## fill absolute frequency values from data
        ## datapoints with missing values are discarded
        ## (note to self: for-loop was faster than 'apply')
        for(arow in 1:nrow(data)){
            datum <- data[arow,]
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


infer <- function(
    agent,
    predictand = NULL,
    predictor = NULL
){
#### Calculates conditional or unconditional probability for new unit
    variates <- names(dimnames(agent[['counts']]))
    ##
    ## Load alpha parameters from "agent" object
    ## length(agent[['counts']]) =: M
    alphas <- agent[['alphas']] / length(agent[['counts']])
    ##
    ## Check if at least one predictor variate is valid
    if(!is.null(predictor) && !any(variates %in% names(predictor))){
        message('Warning: discarding all predictors, none valid')
        predictor <- NULL
    }
    ##
    ## Selection of predictor values, if predictor is given
    if(!is.null(predictor)){ # predictor is given
        predictor <- as.list(predictor)
        ## Check consistency of variates in metadata and predictor
        if(all(variates %in% names(predictor))){
            stop('All variates are in the predictor')
        }
        if(!all(names(predictor) %in% variates)){
            message('Discarding predictor variates not present in metadata')
        }
        predictor <- predictor[variates]
        predictor[lengths(predictor) == 0] <- TRUE
        ## select subarray of counts corresponding to the predictor values
        counts <- do.call(`[`, c(list(agent[['counts']]), predictor))
        if(is.null(dim(counts))){
            dim(counts) <- length(counts)
            dimnames(counts) <- dimnames(agent[['counts']])[
                -which(names(predictor) %in% variates)]
        }
    }else{ # no predictor specified
        counts <- agent[['counts']]
    }
    ##
    ## Selection of predictand variates, if given
    if(!is.null(predictand) && !any(predictand %in% names(dimnames(counts)))){
        message('Discarding all predictands: none matches allowed ones')
        predictand <- NULL
    }
    if(!is.null(predictand)){ # predictand given
        ## Check consistency of variates in metadata and predictand
        if(!all(predictand %in% names(dimnames(counts)))){
            message('Discarding predictands not present in metadata')
        }
        predictand <- predictand[predictand %in% names(dimnames(counts))]
        ##
        ## predictand-index
        ipredictand <- which(names(dimnames(counts)) %in% predictand)
        ## Multiplicative factor for alpha parameters, owing to marginalization
        alphas <- prod(dim(counts)[-ipredictand]) * alphas
        ##
        ## Marginalize frequencies
        counts <- apply(counts, predictand, sum)
        if(is.null(dim(counts))){
            dim(counts) <- length(counts)
            dimnames(counts) <- dimnames(agent[['counts']])[predictand]
        }
    }
    ##
    ## create an array of alphas and counts
    ## then complete with log-probabilities stored in "agent" object
    ## (note to self: aperm+sapply is half as fast)
    counts <- log(outer(alphas, counts, `+`)) + agent[['auxalphas']]
    ##
    temp <- dimnames(counts)[-1] # save dimnames, possibly lost in colSums
    ## Calculate final probability distribution for new unit:
    ## "-max(counts)": renormalize against overflow
    ## "exp()": go from log-probabilities to probabilities
    ## "colSums()": sum over alpha (that is, k)
    counts <- colSums(exp(counts - max(counts)))
    ## Reshape array of results
    if(is.null(dim(counts))){
        dim(counts) <- length(counts)
        dimnames(counts) <- temp
    }
    ## Normalize and output the probability distribution as a vector/array
    counts / sum(counts)
}


rF <- function(
    n = 1,
    agent,
    predictand = NULL,
    predictor = NULL
){
#### Returns a sample of full-population frequency
#### Requires 'extraDistr'
    variates <- names(dimnames(agent[['counts']]))
    ##
    ## Select n alpha parameters according to their probabilities given data
    ## length(agent[['counts']]) =: M
    alphas <- agent[['alphas']] / length(agent[['counts']])
    ## recycling 'alphas'
    alphas <- alphas[sample.int(n = length(alphas), size = n, replace = TRUE,
                                prob = agent[['palphas']])]
    ##
    ##
    ## Selection of predictor values
    ##
    ## Check if at least one predictor variate is valid
    if(!is.null(predictor) && !any(variates %in% names(predictor))){
        message('Warning: discarding all predictors, none valid')
        predictor <- NULL
    }
    if(!is.null(predictor)){ # predictor is given
        predictor <- as.list(predictor)
        ## Check consistency of variates in metadata and predictor
        if(all(variates %in% names(predictor))){
            stop('All variates are in the predictor')
        }
        if(!all(names(predictor) %in% variates)){
            message('Discarding predictor variates not present in metadata')
        }
        predictor <- predictor[variates]
        predictor[lengths(predictor) == 0] <- TRUE
        ## select subarray of counts corresponding to the predictor values
        counts <- do.call(`[`, c(list(agent[['counts']]), predictor))
        if(is.null(dim(counts))){
            dim(counts) <- length(counts)
            dimnames(counts) <- dimnames(agent[['counts']])[
                -which(names(predictor) %in% variates)]
        }
    }else{ # no predictor specified
        counts <- agent[['counts']]
    }
    ##
    ## Selection of predictand variates, if given
    if(!is.null(predictand) && !any(predictand %in% names(dimnames(counts)))){
        message('Discarding all predictands: none matches allowed ones')
        predictand <- NULL
    }
    if(!is.null(predictand)){ # predictand given
        ## Check consistency of variates in metadata and predictand
        if(!all(predictand %in% names(dimnames(counts)))){
            message('Discarding predictands not present in metadata')
        }
        predictand <- predictand[predictand %in% names(dimnames(counts))]
        ##
        ## predictand-index
        ipredictand <- which(names(dimnames(counts)) %in% predictand)
        ## Multiplicative factor for alpha parameters, owing to marginalization
        alphas <- prod(dim(counts)[-ipredictand]) * alphas
        ##
        ## Marginalize frequencies
        counts <- apply(counts, predictand, sum)
        if(is.null(dim(counts))){
            dim(counts) <- length(counts)
            dimnames(counts) <- dimnames(agent[['counts']])[predictand]
        }
    }
    ##
    ## Create n joint-frequency distributions
    ##
    ff <- extraDistr::rdirichlet(n, alpha = outer(alphas, c(counts), `+`))
    ## ff <- nimble::rdirch(n, alpha = outer(alphas, c(counts), `+`))
    dim(ff) <- c(n, dim(counts))
    dimnames(ff) <- c(list(sample = NULL), dimnames(counts))
    ff
}


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
    tplot(y=t(samples), x=1:ncol(samples), type='b',
      xticks=1:ncol(samples), xlabels=dimnames(samples)[[2]],
      xlab=bquote(italic(.(names(dimnames(samples))[2]))),
      ylab='probability',
      lty=1, lwd=1, pch=16, col=7, alpha=0.25, ...
      )
    if(probability){
        fmean <- infer(agent=agent, predictand=predictand, predictor=predictor)
        tplot(y=fmean, x=1:ncol(samples), type='b',
              lty=1, lwd=4, pch=18, col=1, alpha=0.75, add = TRUE
              )
    }
    if(!is.null(file)){
        dev.off()
    }
}


decide <- function(
    probs = NULL,
    utils = NULL
){
#### Makes a decision depending on probabilities and utilities
#### Decisions correspond to ROWS of the utilities array
    ##
    ## Sanity checks for input arguments
    if(is.null(probs) && is.null(utils)){
        stop("Either 'probs' or 'utils' must be given")
    }
    if(is.null(utils)){ # utilities not given: assume accuracy utility
        if(!is.null(dim(probs)) && length(dim(probs)) > 1){
            stop("'probs' is not a vector, please specify array of utilities")
        }
        utils <- diag(length(probs))
        if(!is.null(dimnames(probs))){
            dimnames(utils) <- c(decision = dimnames(probs), dimnames(probs))
        }else{
            if(is.null(names(probs))){
                names(probs) <- 1:length(probs)
            }
            dimnames(utils) <- list(decision = names(probs), outcome = names(probs))
        }
    }else if(is.null(probs)){ # probabilities not given: assume uniform probs
        probs <- rep(1 / ncol(utils), ncol(utils))
        dim(probs) <- ncol(utils)
        if(is.null(dimnames(utils))){
            dimnames(utils) <- list(decision = 1:nrow(utils),
                outcome = 1:ncol(utils))
        }
        dimnames(probs) <- dimnames(utils)[-1]
    }
    ## Check that dimension of probability variates and utilities match
    if(length(probs) != prod(dim(utils)[-1])){
        stop('Mismatch between inference and utility variates')
    }
    if(is.null(dimnames(utils))){
        if(dim(utils)[1] == dim(utils)[2]){
            dimnames(utils) <- c(decision = dimnames(probs), dimnames(probs))
        } else {
            dimnames(utils) <- c(decision = 1:nrow(utils), dimnames(probs))
        }
    } else {
        if(!identical(dimnames(utils)[-1], dimnames(probs))){
            stop('Mismatch in outcomes for utilities and probabilities')
        }
        if(is.null(dimnames(utils)[[1]])){
            if(dim(utils)[1] == dim(utils)[2]){
                dimnames(utils)[1] <- dimnames(probs)
            } else {
                dimnames(utils)[1] <- list(decision = 1:nrow(utils))
            }
        }
    }
    decisions <- dimnames(utils)[[1]] # names of decisions
    ##
    ## If necessary, do some array reshaping for faster computation
    if(length(dim(utils)) > 2 || length(dim(probs)) > 1){
        dim(utils) <- c(dim(utils)[1], prod(dim(utils)[-1]))
        rownames(utils) <- decisions
        dim(probs) <- prod(dim(probs))
    }
    ## Calculate expected utilities (matrix product)
    exputils <- sort((utils %*% probs)[, 1], decreasing = TRUE)
    ## Select one decision with max expected utility
    optimal <- names(exputils)[which(exputils == max(exputils))]
    if(length(optimal) > 1){ # if only one requested, then sample
        optimal <- sample(optimal, size = 1)
    }
    ## Output sorted decisions and one optimal decisions
    list(EUs = exputils, optimal = optimal)
}


mutualinfo <- function(
    agent,
    A,
    B,
    base = 2
){
#### Returns the mutual information between two sets of variates A, B
    ## calculate the joint probability of variates A and B
    probs <- infer(agent = agent, predictand = c(A, B))

    ## load list of variates
    variates <- names(dimnames(probs))
    ## Sanity checks of input arguments
    if(!all(A %in% variates)){
        message('Discarding A variates not present in probs')
        A <- A[A %in% variates]
    }
    if(!all(B %in% variates)){
        message('Discarding B variates not present in probs')
        B <- B[B %in% variates]
    }
    if(any(A %in% B)){
        stop('A and B have variates in common.')
    }
    ## Calculate marginal joint probability for (A,B) if necessary
    ## probs <- apply(probs, c(A, B), sum)
    ## probs <- probs / sum(probs)
    sum(probs * (log2(probs) - log2(outer(
                                   apply(probs, A, sum),
                                   apply(probs, B, sum),
                                   `*`
                               ))), na.rm = TRUE) / log2(base)
}


