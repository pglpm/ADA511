rF <- function(n=1, agent, predictand=NULL, predictor=NULL){
#### Return a sample of full-population frequency
#### Requires 'extraDistr'
    variates <- names(dimnames(agent[['counts']]))
    ##
    ## Selection of predictor values
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
        ipredictor <- match(names(predictor), variates) # predictor-indices
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
    ## recycling 'alphas'
    alphas <- alphas[sample.int(n=length(alphas), size=n, replace=T,
                                      prob=agent[['palphas']])]
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
    ## Create n joint-frequency distributions
    ##
    ff <- extraDistr::rdirichlet(n, alpha=outer(alphas, c(counts), `+`))
    ## ff <- nimble::rdirch(n, alpha=outer(alphas, c(counts), `+`))
    dim(ff) <- c(n,dim(counts))
    dimnames(ff) <- c(list(sample=NULL), dimnames(counts))
    ff
}
