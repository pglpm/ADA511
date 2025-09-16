decide <- function(
    probs = NULL,
    utils = NULL
){
#### Makes a decision depending on probabilities and utilities
#### Decisions correspond to ROWS of the utilities array
    ##
    ## Sanity checks for input arguments
    if(is.null(probs) && is.null(utils)){
        stop("Either 'probs' or 'utils' must be given")
    }
    if(is.null(utils)){ # utilities not given: assume accuracy utility
        if(!is.null(dim(probs)) && length(dim(probs)) > 1){
            stop("'probs' is not a vector, please specify array of utilities")
        }
        utils <- diag(length(probs))
        if(!is.null(dimnames(probs))){
            dimnames(utils) <- c(decision = dimnames(probs), dimnames(probs))
        }else{
            if(is.null(names(probs))){
                names(probs) <- 1:length(probs)
            }
            dimnames(utils) <- list(decision = names(probs), outcome = names(probs))
        }
    }else if(is.null(probs)){ # probabilities not given: assume uniform probs
        probs <- rep(1 / ncol(utils), ncol(utils))
        dim(probs) <- ncol(utils)
        if(is.null(dimnames(utils))){
            dimnames(utils) <- list(decision = 1:nrow(utils),
                outcome = 1:ncol(utils))
        }
        dimnames(probs) <- dimnames(utils)[-1]
    }
    ## Check that dimension of probability variates and utilities match
    if(length(probs) != prod(dim(utils)[-1])){
        stop('Mismatch between inference and utility variates')
    }
    if(is.null(dimnames(utils))){
        if(dim(utils)[1] == dim(utils)[2]){
            dimnames(utils) <- c(decision = dimnames(probs), dimnames(probs))
        } else {
            dimnames(utils) <- c(decision = 1:nrow(utils), dimnames(probs))
        }
    } else {
        if(!identical(dimnames(utils)[-1], dimnames(probs))){
            stop('Mismatch in outcomes for utilities and probabilities')
        }
        if(is.null(dimnames(utils)[[1]])){
            if(dim(utils)[1] == dim(utils)[2]){
                dimnames(utils)[1] <- dimnames(probs)
            } else {
                dimnames(utils)[1] <- list(decision = 1:nrow(utils))
            }
        }
    }
    decisions <- dimnames(utils)[[1]] # names of decisions
    ##
    ## If necessary, do some array reshaping for faster computation
    if(length(dim(utils)) > 2 || length(dim(probs)) > 1){
        dim(utils) <- c(dim(utils)[1], prod(dim(utils)[-1]))
        rownames(utils) <- decisions
        dim(probs) <- prod(dim(probs))
    }
    ## Calculate expected utilities (matrix product)
    exputils <- sort((utils %*% probs)[, 1], decreasing = TRUE)
    ## Select one decision with max expected utility
    optimal <- names(exputils)[which(exputils == max(exputils))]
    if(length(optimal) > 1){ # if only one requested, then sample
        optimal <- sample(optimal, size = 1)
    }
    ## Output sorted decisions and one optimal decisions
    list(EUs = exputils, optimal = optimal)
}
