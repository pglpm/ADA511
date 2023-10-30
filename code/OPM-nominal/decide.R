decide <- function(probs=NULL, utils=NULL, all=FALSE){
#### Make a decision depending on probabilities and utilities
#### Decisions correspond to ROWS of the utilities array
    ##
    ## Sanity checks for input args
    if(is.null(probs) && is.null(utils)){
        stop("Either 'probs' or 'utils' must be given")
    }
    if(is.null(utils)){
        utils <- diag(length(probs))
    }else if(is.null(probs)){
        probs <- rep(1/ncol(utils), ncol(utils))
        dim(probs) <- ncol(utils)
        if(is.null(dimnames(utils))){
            dimnames(utils) <- list(1:nrow(utils), 1:ncol(utils))
        }
        dimnames(probs) <- dimnames(utils)[-1]
    }
    ## Check that dimension of probability variates and utilities match
    if(length(probs) != prod(dim(utils)[-1])){
        stop('Mismatch between inference and utility variates')
    }
    if(is.null(names(utils)) && dim(utils)[1] == dim(utils)[2]){
        dimnames(utils) <- c(decision=dimnames(probs), dimnames(probs))
    }
    decisions <- dimnames(utils)[[1]]
    ##
    ## If necessary, do some array reshaping for faster computation
    if(length(dim(utils)) > 2 || length(dim(probs)) > 1){
        dim(utils) <- c(dim(utils)[1], prod(dim(utils)[-1]))
        rownames(utils) <- decisions
        dim(probs) <- prod(dim(probs))
    }
    exputils <- tcrossprod(utils, rbind(probs))
    decisions <- decisions[which(exputils == max(exputils))]
    if(length(decisions) > 1 && !all){
        decisions <- sample(decisions, size=1)
    }
    decisions
}
