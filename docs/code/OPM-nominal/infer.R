infer <- function(agent, predictand=NULL, predictor=NULL){
#### Calculates conditional or unconditional probability for new unit
    variates <- names(dimnames(agent[['counts']]))
    ##
    ## Load alpha parameters from "agent" object
    ## length(agent[['counts']]) =: M
    alphas <- agent[['alphas']]/length(agent[['counts']])
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
    counts <- colSums(exp(counts-max(counts))) # 
    ## Reshape array of results
    if(is.null(dim(counts))){
        dim(counts) <- length(counts)
        dimnames(counts) <- temp
    }
    ## Normalize and output the probability distribution as a vector/array
    counts/sum(counts)
}
