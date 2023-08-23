hitsvsgain <- function(n, chooseAisA=1, chooseAisB=-11, chooseBisB=0, chooseBisA=0, probsA=c(0.95,0.8,0.95,0.8,0.05,0.2), seed=16){
## chooseAisA=1, chooseAisB=0, chooseBisB=0, chooseBisA=-1, probsA=c(0.6,0.7,0.4,0.3)
    ##
    probsA <- rep(probsA, n)
    threshold <- (chooseBisB-chooseAisB)/(chooseAisA-chooseAisB+chooseBisB-chooseBisA)
    ##
    mlhits <- opmhits <- 0
    mlgain <- opmgain <- 0
    if(!missing(seed)){ set.seed(seed) }
    for(probabilityA in probsA){
        mlchoice <- (if(probabilityA > 0.5){
                         'A'
                     }else if(probabilityA < 0.5){
                         'B'
                     }else{sample(c('A','B'),1)})
        opmchoice <- (if(probabilityA > threshold){
                         'A'
                     }else if(probabilityA < threshold){
                         'B'
                     }else{sample(c('A','B'),1)})
        ##
        trueitem <- sample(c('A','B'), 1, prob=c(probabilityA, 1-probabilityA))
        ##
        if(mlchoice == trueitem){
            mlhits <- mlhits+1
            mlgain <- mlgain + (if(trueitem=='A'){chooseAisA}else{chooseBisB})
        }else{
            mlgain <- mlgain + (if(trueitem=='B'){chooseAisB}else{chooseBisA})
        }
        ##
        if(opmchoice == trueitem){
            opmhits <- opmhits+1
            opmgain <- opmgain + (if(trueitem=='A'){chooseAisA}else{chooseBisB})
        }else{
            opmgain <- opmgain + (if(trueitem=='B'){chooseAisB}else{chooseBisA})
        }
    }
    ##
    cat('\nTrials:', length(probsA))
    cat('\nML: hits', mlhits, '(', signif(mlhits/length(probsA)*100,3), '%)',
        '-- tot gain', mlgain)
    cat('\nOPM: hits', opmhits, '(', signif(opmhits/length(probsA)*100,3), '%)',
        '-- tot gain', opmgain)
    cat('\n\n')
}
