library('data.table')
library('png')
library('foreach')
source('/home/pglpm/work/tplotfunctions.R')

testgain <- function(n=1000, probfail, costfail, gainnofail){
    outc <- c('OK  ','Fail')
    gain <- totfail <- totok <- 0
    spr <- paste0('%',nchar(n),'i')
    spr2 <- paste0('%',max(nchar(n*costfail), nchar(n*gainnofail)),'.0f')
    spr3 <- paste0('%',nchar(signif(costfail*probfail+gainnofail*(1-probfail),2))+2,'.3f')
    cat('\n')
    i <- 0
    cat(
        '\rtrial:', i, ',',
        'outcome:', ' NA ', '--',
        'total fails:', sprintf(spr, totfail), ', ',
        'total OK:', sprintf(spr, totok), ', ',
        'total gain:', sprintf(spr2, gain), ', ',
        '**GAIN PER ITEM:', sprintf(spr3, signif(gain/i,2)),'**'
    )
    ##
    for(i in 1:n){Sys.sleep(0.001)
        fail <- sample(c(TRUE, FALSE), 1, prob=c(probfail, 1-probfail))
        if(fail){
            gain <- gain + costfail
            totfail <- totfail + 1
        }else{
            gain <- gain + gainnofail
            totok <- totok + 1
        }
        cat(
            '\rtrial:', i, ',',
            'outcome:', outc[fail+1], '--',
            'total fails:', sprintf(spr, totfail), ', ',
            'total OK:', sprintf(spr, totok), ', ',
            'total gain:', sprintf(spr2, gain), ', ',
            '**GAIN PER ITEM:', sprintf(spr3, signif(gain/i,2)),'**'
            )
    }
    cat('\n')
}
