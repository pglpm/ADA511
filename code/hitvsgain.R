hitvsgain <- function(n, chooseAandisA=1, chooseAbutisB=-11, chooseBandisB=0, chooseBbutisA=0, Aprobs=c(0.95,0.8,0.95,0.8,0.05,0.2), seed=16){
## chooseAandisA=1, chooseAbutisB=0, chooseBandisB=0, chooseBbutisA=-1, Aprobs=c(0.6,0.7,0.4,0.3)
    ##
    spr <- paste0('%',nchar(n),'i')
    spr2 <- paste0('%',nchar(-n*max(abs(c(chooseAandisA, chooseAbutisB, chooseBandisB, chooseBbutisA)))),'i')
    ##
    Aprobs <- rep(Aprobs, n)
    Athreshold <- (chooseBandisB-chooseAbutisB)/(chooseAandisA-chooseAbutisB+chooseBandisB-chooseBbutisA)
    print(Athreshold)
    ##
    mlhit <- opmhit <- 0
    mlgain <- opmgain <- 0
    ##
    ## cat('\n     ', paste0(rep(' ', nchar(n), collapse='')), ' ')
    ## cat(
    ##     'Classifier', paste0(rep(' ',
    ##                              nchar(n)+nchar(-n*max(abs(c(chooseAandisA, chooseAbutisB, chooseBandisB, chooseBbutisA)))) - nchar('Classifier') + 3,
    ##                              collapse='')), '|',
    ##     'optimal machine', paste0(rep(' ',
    ##                              nchar(n)+nchar(-n*max(abs(c(chooseAandisA, chooseAbutisB, chooseBandisB, chooseBbutisA)))) - nchar('optimal machine') + 3,
    ##                              collapse='')), '|',
    ##     )
    ##
    cat('\n')
    if(!missing(seed)){ set.seed(seed) }
    for(trial in 1:n){
        Aprobability <- Aprobs[trial]
        mlchoice <- (if(Aprobability >= 0.5){'A'}else{'B'})
        opmchoice <- (if(Aprobability >= Athreshold){'A'}else{'B'})
        ##
        trueitem <- sample(c('A','B'), 1, prob=c(Aprobability, 1-Aprobability))
        ##
        if(mlchoice == trueitem){
            mlhit <- mlhit+1
            mlgain <- mlgain + (if(trueitem=='A'){chooseAandisA}else{chooseBandisB})
        }else{
            mlgain <- mlgain + (if(trueitem=='B'){chooseAbutisB}else{chooseBbutisA})
        }
        ##
        if(opmchoice == trueitem){
            opmhit <- opmhit+1
            opmgain <- opmgain + (if(trueitem=='A'){chooseAandisA}else{chooseBandisB})
        }else{
            opmgain <- opmgain + (if(trueitem=='B'){chooseAbutisB}else{chooseBbutisA})
        }
        ##
        cat('\rTrial', sprintf(spr, trial), trueitem, '|',
            'ML: hits', sprintf(spr, mlhit),
            '-- gain', sprintf(spr2, mlgain), '|',
            'Opt: hits', sprintf(spr, opmhit),
            '-- gain', sprintf(spr2, opmgain)
            )
    }
    cat('\n\n')
}
