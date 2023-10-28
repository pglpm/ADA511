forecastP <- function(P, predictand=NULL, predictor=NULL, Pout=FALSE){
#### Calculate conditional or unconditional probability
    variates <- names(dimnames(P[['freqs']]))
    M <- length(P[['freqs']])
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
    ## create an array of forecast variates and alphas
    freqs <- aperm(
        sapply(P[['alphas']], function(alpha){
            log(alpha + freqs)
        }, simplify='array'),
        c(length(dim(freqs))+1, 1:length(dim(freqs)))
    )
    freqs <- freqs + P[['valphas']]
    freqs <- freqs - max(freqs)
    ##
    temp <- dimnames(freqs)[-1]
    freqs <- colSums(exp(freqs))
    if(is.null(dim(freqs))){
        dim(freqs) <- length(freqs)
        dimnames(freqs) <- temp
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
        ## Marginalize frequencies
        freqs <- apply(freqs, predictand, sum)
        if(is.null(dim(freqs))){
            dim(freqs) <- length(freqs)
            dimnames(freqs) <- dimnames(P[['freqs']])[predictand]
        }
    }
    freqs/sum(freqs)
}
