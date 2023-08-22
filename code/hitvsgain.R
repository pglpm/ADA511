hitvsgain <- function(n, catyes=1, catno=-1, dogyes=1, dogno=0, probs=c(0.6,0.7,0.4,0.3)){





    for(i in 1:n){
        for(j in )
    }
}


                      failprob, failcost, passgain, seed=17){
    ## for tabulated printing
    spr <- paste0('%',nchar(n),'i')
    spr2 <- paste0('%',max(nchar(n*failcost), nchar(n*passgain)),'.0f')
    spr3 <- paste0('%',nchar(signif(failcost*failprob+passgain*(1-failprob),2))+2,'.3f')
    ## define and initialize some parameters
    outc <- c('pass','fail')
    gain <- totfail <- totok <- 0
    ## set seed, if given, for reproducibility
    if(!missing(seed)){ set.seed(seed) }
    cat('\n')
    for(i in 1:n){
        ## ## slow it down a little just to make it more exciting
        ## Sys.sleep(7/(n*4/100000))
        fail <- sample(c(TRUE, FALSE), 1, prob=c(failprob, 1-failprob))
        if(fail){
            gain <- gain + failcost
            totfail <- totfail + 1
        }else{
            gain <- gain + passgain
            totok <- totok + 1
        }
        cat(
            '\rItem', i,
            outc[fail+1], '|',
            'Tot fail/pass', paste0(sprintf(spr, totfail), '/',
            sprintf(spr, totok)), '|',
            'Tot gain', sprintf(spr2, gain), '|',
            'GAIN/ITEM', sprintf(spr3, signif(gain/i,2))
            )
    }
    cat('\n')
}
