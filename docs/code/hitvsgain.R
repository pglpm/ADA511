hitvsgain <- function(n, choosecatok=1, choosecatbutisdog=-1, choosedogok=1, choosedogbutiscat=0, catprobs=c(0.6,0.7,0.4,0.3), seed=16){
    ##
    spr <- paste0('%',nchar(n),'i')
    spr2 <- paste0('%',nchar(-n*max(abs(c(choosecatok, choosecatbutisdog, choosedogok, choosedogbutiscat)))),'i')
    ##
    catprobs <- rep(catprobs, n)
    catthreshold <- (choosedogok-choosecatbutisdog)/(choosecatok-choosecatbutisdog+choosedogok-choosedogbutiscat)
    print(catthreshold)
    ##
    mlhit <- opmhit <- 0
    mlgain <- opmgain <- 0
    ##
    ## cat('\n     ', paste0(rep(' ', nchar(n), collapse='')), ' ')
    ## cat(
    ##     'Classifier', paste0(rep(' ',
    ##                              nchar(n)+nchar(-n*max(abs(c(choosecatok, choosecatbutisdog, choosedogok, choosedogbutiscat)))) - nchar('Classifier') + 3,
    ##                              collapse='')), '|',
    ##     'optimal machine', paste0(rep(' ',
    ##                              nchar(n)+nchar(-n*max(abs(c(choosecatok, choosecatbutisdog, choosedogok, choosedogbutiscat)))) - nchar('optimal machine') + 3,
    ##                              collapse='')), '|',
    ##     )
    ##
    cat('\n')
    if(!missing(seed)){ set.seed(seed) }
    for(trial in 1:n){
        catprobability <- catprobs[trial]
        mlchoice <- (if(catprobability >= 0.5){'cat'}else{'dog'})
        opmchoice <- (if(catprobability >= catthreshold){'cat'}else{'dog'})
        ##
        trueanimal <- sample(c('cat','dog'), 1, prob=c(catprobability, 1-catprobability))
        ##
        if(mlchoice == trueanimal){
            mlhit <- mlhit+1
            mlgain <- mlgain + (if(trueanimal=='cat'){choosecatok}else{choosedogok})
        }else{
            mlgain <- mlgain + (if(trueanimal=='dog'){choosecatbutisdog}else{choosedogbutiscat})
        }
        ##
        if(opmchoice == trueanimal){
            opmhit <- opmhit+1
            opmgain <- opmgain + (if(trueanimal=='cat'){choosecatok}else{choosedogok})
        }else{
            opmgain <- opmgain + (if(trueanimal=='dog'){choosecatbutisdog}else{choosedogbutiscat})
        }
        ##
        cat('\rTrial', sprintf(spr, trial), trueanimal, '|',
            'ML: hits', sprintf(spr, mlhit),
            '-- gain', sprintf(spr2, mlgain), '|',
            'Opt: hits', sprintf(spr, opmhit),
            '-- gain', sprintf(spr2, opmgain)
            )
    }
    cat('\n\n')
}
