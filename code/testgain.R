testgain <- function(n=1000, failprob, failcost, passgain, seed=17){
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
    for(item in 1:n){
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
            '\rItem', item,
            outc[fail+1], '|',
            'Tot fail/pass', paste0(sprintf(spr, totfail), '/',
            sprintf(spr, totok)), '|',
            'Tot gain', sprintf(spr2, gain), '|',
            'GAIN/ITEM', sprintf(spr3, signif(gain/item,2))
            )
    }
    cat('\n== Final gain/item:', signif(gain/n,2),'==\n\n')
}
