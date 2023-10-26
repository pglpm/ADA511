rF <- function(n=1, P, predictand=NULL, predictor=NULL){
#### Return a sample of full-population frequency
#### Requires 'extraDistr'
    variates <- names(dimnames(P[['freqs']]))
    ##
    ## Selection of predictor values
    ## select subarray of freqs corresponding to the predictor values
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
        totake <- as.list(rep(TRUE, length(variates)))
        totake[ipredictor] <- predictor
        freqs <- do.call(`[`, c(list(P[['freqs']]), totake))
        if(is.null(dim(freqs))){
            dim(freqs) <- length(freqs)
            dimnames(freqs) <- dimnames(P[['freqs']])[-ipredictor]
        }
    }else{
        freqs <- P[['freqs']]
    }
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
        ## Multiplicative factor for alphas
        ipredictand <- match(predictand, variates)
        multalpha <- prod(dim(freqs)[-ipredictand])
        ## Marginalize frequencies
        freqs <- apply(freqs, predictand, sum)
        if(is.null(dim(freqs))){
            dim(freqs) <- length(freqs)
            dimnames(freqs) <- dimnames(P[['freqs']])[predictand]
        }
    }else{
        multalpha <- 1
    }
    ##
    alphasample <- multalpha * sample(rep(P[['alphas']],2), size=n, replace=T,
                                      prob=rep(P[['palphas']],2))
    ##
    ff <- extraDistr::rdirichlet(n, alpha=outer(alphasample, c(freqs), `+`))
    ## ff <- nimble::rdirch(n, alpha=outer(alphasample, c(freqs), `+`))
    dim(ff) <- c(n,dim(freqs))
    dimnames(ff) <- c(list(sample=NULL), dimnames(freqs))
    ff
}
