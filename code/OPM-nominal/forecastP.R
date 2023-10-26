forecastP <- function(P, marginal=NULL, conditional=NULL, Pout=FALSE){
#### Calculate conditional or unconditional probability
    variates <- names(dimnames(P[['freqs']]))
    M <- length(P[['freqs']])
    ##
    ## Selection of conditional values
    ## select subarray of freqs corresponding to the conditional values
    if(!is.null(conditional)){
        conditional <- as.list(conditional)
        ## Check consistency of variates in metadata and conditional
        if(length(setdiff(variates, names(conditional))) == 0){
            stop('All variates are in the conditional')
        }
        if(length(setdiff(names(conditional), variates)) > 0){
            message('Discarding conditional variates not present in metadata')
        }
        conditional <- conditional[na.omit(match(variates, names(conditional)))]
        ##
        iconditional <- match(names(conditional), variates)
        totake <- as.list(rep(TRUE, length(variates)))
        totake[iconditional] <- conditional
        freqs <- do.call(`[`, c(list(P[['freqs']]), totake))
        if(is.null(dim(freqs))){
            dim(freqs) <- length(freqs)
            dimnames(freqs) <- dimnames(P[['freqs']])[-iconditional]
        }
    }else{
        freqs <- P[['freqs']]
    }
   ##
    ## create an array of forecast variates and alphas
    freqs <- aperm(
        sapply(P[['alphas']], function(alpha){
            log(M*alpha + freqs)
        }, simplify='array'),
        c(length(dim(freqs))+1, 1:length(dim(freqs)))
    )
    freqs <- freqs - max(freqs) + P[['valphas']]
    ##
    temp <- dimnames(freqs)[-1]
    freqs <- colSums(exp(freqs))
    if(is.null(dim(freqs))){
        dim(freqs) <- length(freqs)
        dimnames(freqs) <- temp
        }
    ##
    ## Selection of marginal variates
    if(!is.null(marginal)){
        ## Check consistency of variates in metadata and marginal
        if(length(setdiff(marginal, variates)) > 0){
            message('Discarding marginal variates not present in metadata')
        }
        marginal <- marginal[na.omit(match(variates, marginal))]
        ## Check consistency of variates in conditional and marginal
        if(length(intersect(marginal, names(conditional))) > 0){
            stop('Some variates appear in marginal and conditional')
        }
        ##
        ## Marginalize frequencies
        freqs <- apply(freqs, marginal, sum)
        if(is.null(dim(freqs))){
            dim(freqs) <- length(freqs)
            dimnames(freqs) <- dimnames(P[['freqs']])[marginal]
        }
    }
    freqs/sum(freqs)
}
