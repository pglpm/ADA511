forecast <- function(agent, predictand=NULL, predictor=NULL){
#### Calculate conditional or unconditional probability
    variates <- names(dimnames(agent[['counts']]))
    ##
    ## Selection of predictor values, if given
    ##
    ## Check if at least one predictor variate is valid
    if(!is.null(predictor) && length(intersect(variates, names(predictor))) == 0){
        message('Warning: discarding all predictors, none valid')
        predictor <- NULL
    }
    if(!is.null(predictor)){
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
    }else{
        ## no predictors specified
        counts <- agent[['counts']]
    }
    ##
    ## Select n alpha parameters according to their probabilities given data
    ## length(agent[['counts']]) =: M
    alphas <- agent[['alphas']]/length(agent[['counts']])
    ##
    ## Selection of predictand variates
    if(!is.null(predictand)){
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
        ipredictand <- match(predictand, variates) # predictand-index
        ## Multiplicative factor for alpha samples, owing to marginalization
        alphas <- prod(dim(counts)[-ipredictand]) * alphas
        ##
        ## Marginalize frequencies
        counts <- apply(counts, ipredictand, sum)
        if(is.null(dim(counts))){
            dim(counts) <- length(counts)
            dimnames(counts) <- dimnames(agent[['counts']])[ipredictand]
        }
    }
    ##
    ## create an array of alphas and counts
    counts <- outer(alphas, counts, `+`)
    ##
    ## ## (note-to-self: aperm+sapply is half as fast)
    ## counts <- log(aperm(
    ##     sapply(agent[['alphas']], function(alpha){
    ##         alpha + counts
    ##     }, simplify='array'),
    ##     c(length(dim(counts))+1, 1:length(dim(counts)))
    ## ))
    counts <- log(counts) + agent[['auxalphas']]
    ##
    temp <- dimnames(counts)[-1]
    counts <- colSums(exp(counts-max(counts)))
    if(is.null(dim(counts))){
        dim(counts) <- length(counts)
        dimnames(counts) <- temp
        }
    counts/sum(counts)
}
