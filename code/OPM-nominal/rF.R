rF <- function(n=1, K){
#### Return a sample of full-population frequency
#### Requires 'extraDistr'
    alphasample <- sample(rep(K[['alphas']],2), size=n, replace=T,
                          prob=rep(K[['palphas']],2))
    ff <- extraDistr::rdirichlet(n, alpha=outer(alphasample, c(K[['freqs']]), `+`))
    dim(ff) <- c(n,dim(K[['freqs']]))
    dimnames(ff) <- c(list(sample=NULL), dimnames(K[['freqs']]))
    ff
}
