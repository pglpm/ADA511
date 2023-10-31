infer <- function(agent, predictand=NULL, predictor=NULL){
#### Calculate conditional or unconditional probability for new unit
    variates <- names(dimnames(agent[['counts']]))
    ##
    ## Check if at least one predictor variate is valid
    if(!is.null(predictor) && length(intersect(variates, names(predictor))) == 0){
        message('Warning: discarding all predictors, none valid')
        predictor <- NULL
    }
    ##
    ## Selection of predictor values, if predictor is given
    if(!is.null(predictor)){ # predictor is given
        predictor <- as.list(predictor)
        ## Check consistency of variates in metadata and predictor
        if(length(setdiff(variates, names(predictor))) == 0){
            stop('All variates are in the predictor')
        }
        if(length(setdiff(names(predictor), variates)) > 0){
            message('Discarding predictor variates not present in metadata')
        }
        predictor <- predictor[na.omit(match(variates, names(predictor)))]
        ##
        ipredictor <- match(names(predictor), variates)
        ## select subarray of counts corresponding to the predictor values
        totake <- as.list(rep(TRUE, length(variates)))
        totake[ipredictor] <- predictor
        counts <- do.call(`[`, c(list(agent[['counts']]), totake))
        if(is.null(dim(counts))){
            dim(counts) <- length(counts)
            dimnames(counts) <- dimnames(agent[['counts']])[-ipredictor]
        }
    }else{ # no predictor specified
        counts <- agent[['counts']]
    }
    ##
    ## Load alpha parameters from "agent" object
    ## length(agent[['counts']]) =: M
    alphas <- agent[['alphas']]/length(agent[['counts']])
    ##
    ## Selection of predictand variates, if given
    if(!is.null(predictand)){ # predictand given
        ## Check consistency of variates in metadata and predictand
        if(length(setdiff(predictand, variates)) > 0){
            message('Discarding predictand variates not present in metadata')
        }
        predictand <- predictand[na.omit(match(variates, predictand))]
        ## Check consistency of variates in predictor and predictand
        if(length(intersect(predictand, names(predictor))) > 0){
            stop('Some variates appear in predictand and predictor')
        }
        ##
        ipredictand <- match(predictand, names(dimnames(counts))) # predictand-index
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
