decide <- function(probs, utils=NULL, onlyone=T){
#### Make a decision depending on probabilities and utilities
#### Decisions correspond to ROWS of the utilities array
    ##
    if(is.null(utils)){
        utils <- diag(length(probs))
        rownames(utils) <- names(testf)
    }
    ## Check that dimension of probability variates and utilities match
    if(length(probs) != prod(dim(utils)[-1])){
        stop('Mismatch between inference and utility variates')
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
    if(length(decisions) > 1 && onlyone){
        decisions <- sample(decisions, size=1)
    }
    decisions
}
