hitsvsgain <- function(ntrials, chooseAtrueA, chooseAtrueB, chooseBtrueB, chooseBtrueA, probsA=0.5){
    ## Recycle the given probabilities for the number of trials
    probsA <- rep(probsA, ntrials)[1:ntrials]
    ## "Magic" parameter used in making the optimal decision
    threshold <- (chooseBtrueB-chooseAtrueB)/(chooseAtrueA-chooseAtrueB+chooseBtrueB-chooseBtrueA)
    ## Initialize total "hits" and gains
    ## 'mlc' refers to the Machine-Learning Classifier
    ## 'opm' refers to the Optimal Predictor Machine
    mlchits <- mlcgain <- 0
    opmhits <- opmgain <- 0
    ##
    ## Loop through the trials
    for(probabilityA in probsA){
        ## Output of the MLC, based on the current probability
        mlcchoice <- (if(probabilityA > 0.5){
                         'A'
                     }else if(probabilityA < 0.5){
                         'B'
                     }else{sample(c('A','B'),1)})
        ## Output of the OPM, based on the current probability
        opmchoice <- (if(probabilityA > threshold){
                         'A'
                     }else if(probabilityA < threshold){
                         'B'
                     }else{sample(c('A','B'),1)})
        ##
        ## Correct answer for the current trial
        trueitem <- sample(c('A','B'), 1, prob=c(probabilityA, 1-probabilityA))
        ##
        ## MLC: add one "hit" if correct guess, and add gain/loss
        if(mlcchoice == trueitem){
            mlchits <- mlchits+1
            mlcgain <- mlcgain + (if(trueitem=='A'){chooseAtrueA}else{chooseBtrueB})
        }else{
            mlcgain <- mlcgain + (if(trueitem=='B'){chooseAtrueB}else{chooseBtrueA})
        }
        ##
        ## OPM: add one "hit" if correct guess, and add gain/loss
        if(opmchoice == trueitem){
            opmhits <- opmhits+1
            opmgain <- opmgain + (if(trueitem=='A'){chooseAtrueA}else{chooseBtrueB})
        }else{
            opmgain <- opmgain + (if(trueitem=='B'){chooseAtrueB}else{chooseBtrueA})
        }
    }
    ## end of loop
    ##
    ## Output total number of hits and total gain or loss produced
    cat('\nTrials:', length(probsA))
    cat('\nMLC: hits', mlchits, '(', signif(mlchits/length(probsA)*100,3), '%)',
        '-- tot gain', mlcgain)
    cat('\nOPM: hits', opmhits, '(', signif(opmhits/length(probsA)*100,3), '%)',
        '-- tot gain', opmgain)
    cat('\n\n')
}
