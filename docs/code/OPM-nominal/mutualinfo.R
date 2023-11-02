mutualinfo <- function(probs, A, B, base=2){
#### Returns the mutual information between two sets of variates A, B
    ## load list of variates
    variates <- names(dimnames(probs))
    ## Sanity checks of input arguments
    if(!all(A %in% variates)){
        message('Discarding A variates not present in probs')
        A <- A[A %in% variates]
    }
    if(!all(B %in% variates)){
        message('Discarding B variates not present in probs')
        B <- B[B %in% variates]
    }
    if(any(A %in% B)){
        stop('A and B have variates in common.')
    }
    ## Calculate marginal joint probability for (A,B) if necessary
    probs <- apply(probs, c(A,B), sum)
    probs <- probs/sum(probs)
    sum(probs * (log2(probs) - log2(outer(
                                   apply(probs, A, sum),
                                   apply(probs, B, sum),
                                   `*`
                               ))), na.rm=TRUE)/log2(base)
}
