hitsvsgain <- function(ntrials, chooseAtrueA, chooseAtrueB, chooseBtrueB, chooseBtrueA, probsA=0.5){
    ## Recycle & shuffle the given probabilities for the number of trials
    probsArepeated <- sample(probsA, ntrials, replace=TRUE)
    ## "Magic" parameter used in making the optimal decision
    threshold1 <- (chooseAtrueA-chooseAtrueB+chooseBtrueB-chooseBtrueA)
    threshold2 <- (chooseBtrueB-chooseAtrueB)
    ## Initialize total "hits" and gains
    ## 'mlc' refers to the Machine-Learning Classifier
    ## 'opm' refers to the Optimal Predictor Machine
    mlchits <- 0
    mlcgain <- 0
    opmhits <- 0
    opmgain <- 0
    ##
    ## Loop through the trials and their probabilities
    for(probabilityA in probsArepeated){
        ## Output of the MLC, based on the current probability
        if(probabilityA > 0.5){
            mlcchoice <- 'A'
        }else if(probabilityA < 0.5){
            mlcchoice <- 'B'
        }else{
            mlcchoice <- sample(c('A','B'), 1) # A or B with 50%/50% prob.
        }
        ## Output of the OPM, based on the current probability
        if(threshold1 * probabilityA > threshold2){
            opmchoice <- 'A'
        }else if(threshold1 * probabilityA < threshold2){
            opmchoice <- 'B'
        }else{
            opmchoice <- sample(c('A','B'), 1) # A or B with 50%/50% prob.
        }
        ##
        ## Correct answer for the current trial
        trueitem <- sample(c('A','B'), 1, prob=c(probabilityA, 1-probabilityA))
        ##
        ## MLC: add one "hit" if correct guess, and add gain/loss
        if(mlcchoice == trueitem){
            mlchits <- mlchits + 1 # one success
            if(trueitem=='A'){
                mlcgain <- mlcgain + chooseAtrueA
            }else{
                mlcgain <- mlcgain + chooseBtrueB
            }
        }else{
            if(trueitem=='B'){
                mlcgain <- mlcgain + chooseAtrueB
            }else{
                mlcgain <- mlcgain + chooseBtrueA
            }
        }
        ##
        ## OPM: add one "hit" if correct guess, and add gain/loss
        if(opmchoice == trueitem){
            opmhits <- opmhits + 1 # one success
            if(trueitem=='A'){
                opmgain <- opmgain + chooseAtrueA
            }else{
                opmgain <- opmgain + chooseBtrueB
            }
        }else{
            if(trueitem=='B'){
                opmgain <- opmgain + chooseAtrueB
            }else{
                opmgain <- opmgain + chooseBtrueA
            }
        }
    }
    ## end of loop
    ##
    ## Output total number of hits and total gain or loss produced
    cat('\nTrials:', ntrials)
    cat('\nMachine-Learning Classifier: successes', mlchits, '(', signif(mlchits/ntrials*100, 3), '%)',
        '| total gain', mlcgain)
    cat('\nOptimal Predictor Machine:   successes', opmhits, '(', signif(opmhits/ntrials*100, 3), '%)',
        '| total gain', opmgain)
    cat('\n\n')
}
