forecastK <- function(K, conditional=NULL){
#### Calculate conditional or unconditional probability
    variates <- names(dimnames(K[['freqs']]))
    SS <- length(K[['freqs']])
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
        freqs <- do.call(`[`, c(list(K[['freqs']]), totake))
        if(is.null(dim(freqs))){
            dim(freqs) <- length(freqs)
            dimnames(freqs) <- dimnames(K[['freqs']])[-iconditional]
        }
    }else{
        freqs <- K[['freqs']]
    }
    ##
    ## create an array of forecast variates and alphas
    freqs <- aperm(
        sapply(K[['alphas']], function(alpha){
            log(SS*alpha + freqs)
        }, simplify='array'),
        c(length(dim(freqs))+1, 1:length(dim(freqs)))
    )
    freqs <- freqs - max(freqs) + K[['valphas']]
    ##
    temp <- dimnames(freqs)[-1]
    freqs <- colSums(exp(freqs))
    if(is.null(dim(freqs))){
        dim(freqs) <- length(freqs)
        dimnames(freqs) <- temp
        }
    freqs/sum(freqs)
}
