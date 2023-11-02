rF <- function(n=1, agent, predictand=NULL, predictor=NULL){
#### Returns a sample of full-population frequency
#### Requires 'extraDistr'
    variates <- names(dimnames(agent[['counts']]))
    ##
    ## Select n alpha parameters according to their probabilities given data
    ## length(agent[['counts']]) =: M
    alphas <- agent[['alphas']]/length(agent[['counts']])
    ## recycling 'alphas'
    alphas <- alphas[sample.int(n=length(alphas), size=n, replace=T,
                                      prob=agent[['palphas']])]
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
            dimnames(counts) <- dimnames(agent[['counts']])[-which(names(predictor) %in% variates)]
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
        ipredictand <- which(names(dimnames(counts)) %in% predictand) # predictand-index
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
    ff <- extraDistr::rdirichlet(n, alpha=outer(alphas, c(counts), `+`))
    ## ff <- nimble::rdirch(n, alpha=outer(alphas, c(counts), `+`))
    dim(ff) <- c(n,dim(counts))
    dimnames(ff) <- c(list(sample=NULL), dimnames(counts))
    ff
}
