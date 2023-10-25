rF <- function(n=1, P){
#### Return a sample of full-population frequency
#### Requires 'extraDistr'
    alphasample <- sample(rep(P[['alphas']],2), size=n, replace=T,
                          prob=rep(P[['palphas']],2))
    ff <- extraDistr::rdirichlet(n, alpha=outer(alphasample, c(P[['freqs']]), `+`))
    dim(ff) <- c(n,dim(P[['freqs']]))
    dimnames(ff) <- c(list(sample=NULL), dimnames(P[['freqs']]))
    ff
}
