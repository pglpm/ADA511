#### Guess metadata information from dataset
#### and save it in a metadata file
guessmetadata <- function(
    data,
    file = NULL
){
    if(is.character(data)){
        if(is.null(file)){
            file <- paste0('meta_', data)
        }
        data <- read.csv(data,
            na.strings = '', stringsAsFactors = FALSE, tryLogical = FALSE,
            header = TRUE)
    } else {
        if(is.null(file)){
            cat("\n'file' argument missing: output to stdout\n")
        }
    }

    nvariates <- ncol(data)
    nn <- sapply(data, function(xx){length(unique(xx))})
    maxN <- max(nn)
    ## ## for debugging
    ## str(matrix(NA, nrow=nvariates,ncol=2+maxN, dimnames=list(NULL, c('variate','domainsize',paste0('V',1:maxN)))))
    metadata <- as.data.frame(matrix(character(),
        nrow = nvariates, ncol = 2 + maxN,
        dimnames = list(NULL,
            c('variate', 'domainsize', paste0('V', 1:maxN)))
    ))
    ## ## for debugging
    ## print(metadata)
    metadata[['variate']] <- colnames(data)
    metadata[['domainsize']] <- nn
    for(i in 1:nvariates){
        metadata[i, paste0('V', 1:nn[i])] <- as.list(sort(unique(data[[i]])))
    }

    if(!is.null(file)){
        write.csv(x = metadata, file = file,
            row.names = FALSE, quote = TRUE, na = '')
    } else {
        metadata
    }
}


#### Builds "agent" object encoding background & learned knowledge
#### Requires package 'collapse'
buildagent <- function(
    metadata,
    data = NULL,
    kmi = 0,
    kma = 20,
    base = 2,
    alphas = NULL,
    savememory = FALSE
){
    ## Read metadata from file, if given as file
    if(is.character(metadata)){
        metadata <- read.csv(metadata,
            na.strings='', stringsAsFactors = FALSE, tryLogical = FALSE)
    }

    variatenames <- metadata$variate
    nvariates <- length(variatenames)
    domainsizes <- metadata$domainsize
    names(domainsizes) <- variatenames
    M <- prod(domainsizes) # number of possible joint values

    ## put all variate values in a list
    variates <- setNames(object = apply(metadata, 1,
        function(metadatum){unname(
            metadatum[paste0('V', seq_len(metadatum['domainsize']))]
        )}, simplify = list),
        nm = variatenames)


    ## Handling & checks of dataset

    ## Load training data, if given as file
    if(is.character(data)){
        data <- read.csv(data,
            na.strings='', stringsAsFactors = FALSE, tryLogical = FALSE)
    }

    ## stop if variates are missing in data
    if(!all(variatenames %in% colnames(data))){
        stop('Missing variates in data.')
    }
    ## reorder data columns according to metadata variates
    ## remove variates not given in metadata, with warning
    if(!all(colnames(data) %in% variatenames)){
        message('Discarding data variates not given in metadata.')
    }
    data <- data[, variatenames, drop = FALSE]
    ## remove datapoints with missing values
    tokeep <- complete.cases(data)
    N <- sum(tokeep) # total number of learning data
    if(any(!tokeep)){
        message('Removing datapoints with missing values: ',
            N, ' data left.')
        if(all(!tokeep)){ # no data left
            data <- NULL
        } else {
            data <- data[tokeep, , drop = FALSE]
        }
    }


    ## Handling of 2^k in Dirichlet-mixture distribution
    ## each "2^k" is called "alpha"

    if(is.null(alphas)){
        alphas <- base^(kmi:kma) # 2^kmin to 2^kmax
    } else if(isTRUE(alphas)){
        alphas <- sqrt(M) # other alternative with just one k
    }

    ## Possibility of adding a p(k) weight in the Dirichlet-mixture distribution
    ## logpalphas0 <- -abs(alphas) # Good's hyperprior

    ## Calculation and Storage of counts in learning dataset
    ## Two methodos:
    ## savememory == TRUE  ->  smart storage
    ## savememory == FALSE  ->  store all
    if(savememory){
        agentclass <- 'agentcompressed'

        ## list unique joint values in data
        ## count occurrence of each unique value
        if(!is.null(data)){
            uniquedata <- collapse::fcount(data)
            ## extract counts, stored in last column
            counts <- uniquedata[, ncol(uniquedata)]
            ncounts <- nrow(uniquedata)
            ## remove counts column
            uniquedata <- uniquedata[, -ncol(uniquedata), drop = FALSE]
        } else {
            uniquedata <- as.data.frame(
                rbind(variatenames))[-1, , drop = FALSE]
            counts <- numeric(0)
            ncounts <- 0L
        }

        ## store how many times each different count value appears
        freqscounts <- c(M - ncounts, tabulate(counts))

    } else {
        agentclass <- 'agent'

        ## check which variate values are not present in data
        missingvals <- Filter(length,
            sapply(variatenames, function(var){
                variates[[var]][!(variates[[var]] %in% data[,var])]
            })
        )
        temp <- data.frame(matrix(NA_character_,
            sum(lengths(missingvals)), nvariates))
        colnames(temp) <- variatenames

        i <- 0
        for(var in names(missingvals)){
            toadd <- missingvals[[var]]
            temp[i + seq_along(toadd), var] <- toadd
            i <- i + length(toadd)
        }

        ## count occurrences of all possible joint values
        ## counts := #z for all values of z
        counts <- table(rbind(data, temp))

        ## store how many times each different count value appears
        freqscounts <- tabulate(c(counts) + 1L)

        ## reorder variate values so they match those in counts
        variates <- dimnames(counts)
        uniquedata <- NULL
    }


    ## For faster computation, remove counts that never appear
    nonzerofreqs <- which(freqscounts > 0)
    freqscounts <- freqscounts[nonzerofreqs]
    countsvalues <- nonzerofreqs - 1L ## first count is for 0

    ## calculate log(aux(k)) for all k (alphas)
    ## lgamma(...) := log(factorial(... - 1))
    logauxk <- colSums(
        freqscounts * lgamma(outer(countsvalues, alphas / M, `+`))
    ) - lgamma(alphas + N + 1) + lgamma(alphas) -
        M * lgamma(alphas / M)  # + logpalphas0


    ## palphas := log-probability distribution for alpha parameters
    ## used for population-frequency forecasts
    palphas <- logauxk + log(alphas + N)
    palphas <- exp(palphas - max(palphas)) # avoid overflow
    palphas <- palphas / sum(palphas)

    ## Output "agent" object
    out <- list(
        uniquedata = uniquedata,
        counts = counts,
        variates = variates,
        alphas = alphas,
        logauxk = logauxk,
        palphas = palphas
    )
    class(out) <- agentclass
    out
}

infer <- function(agent, ...) {
    UseMethod("infer")
}

infer.agentcompressed <- function(
    agent,
    predictand,
    predictor = NULL
){
#### Calculates conditional or unconditional probability for new unit
    with(agent, {
        variatenames <- names(variates)
        dimensions <- lengths(variates)

        ## Check validity of predictands
        predictand <- unlist(predictand)
        if(!all(predictand %in% variatenames)){
            stop('Unknown predictands')
        }

        ## Check if at least one predictor variate is valid
        if(!is.null(predictor)){
            predictor <- as.list(predictor)
            namespredictor <- names(predictor)

            if(!all(namespredictor %in% variatenames)){
                message('Discarding predictor variates not given in metadata.')
                predictor <- predictor[namespredictor %in% variatenames]
            }
            if(all(variatenames %in% namespredictor)){
                stop('All variates are in the predictor')
            }
            if(length(predictor) == 0){
                message('Warning: discarding all predictors, none valid')
                predictor <- NULL
            }
        } else {
            namespredictor <- NULL
        }

        ## select subarray of counts corresponding to the predictor values
        if(!is.null(predictor)){
            keeprows <- apply(uniquedata[, namespredictor, drop = FALSE], 1,
                function(x) all(x == predictor))

            uniquedata <- uniquedata[keeprows, predictand, drop = FALSE]
            counts <- counts[keeprows]
        }

        ## prepare a probability array
        probs <- numeric(prod(dimensions[predictand]))
        dim(probs) <- dimensions[predictand]
        dimnames(probs) <- variates[predictand]
        out <- probs

        ## fill count values in probability array
        uniquedata <- as.matrix(uniquedata[, predictand, drop = FALSE])

        for(i in seq_along(counts)){
            datum <- uniquedata[i, , drop = FALSE]
            probs[datum] <- probs[datum] + counts[i]
        }


        ## Multiplicative factor for alpha parameters, owing to marginalization
        alphas <- alphas / prod(dimensions[c(predictand, namespredictor)])

        ## reduce value of logauxk by maximum, to avoid overflow
        logauxk <- logauxk - max(log(alphas + max(probs)) + logauxk)

        ## Calculate final probabilities for predictands
            for(i in seq_along(alphas)){
                out <- out + exp(log(alphas[i] + probs) + logauxk[i])
            }
        ## } else {
        ##     out <- colSums(exp(log(outer(alphas, probs, `+`)) + logauxk))
        ##     dim(out) <- dimensions[predictand]
        ##     dimnames(out) <- variates[predictand]
        ## }

        out / sum(out)
    })
}


infer.agent <- function(
    agent,
    predictand,
    predictor = NULL
){
#### Calculates conditional or unconditional probability for new unit
    with(agent, {
        variatenames <- names(variates)
        dimensions <- lengths(variates)

        ## Check validity of predictands
        predictand <- unlist(predictand)
        if(!all(predictand %in% variatenames)){
            stop('Unknown predictands')
        }

        ## Check if at least one predictor variate is valid
        if(!is.null(predictor)){
            predictor <- as.list(predictor)
            namespredictor <- names(predictor)

            if(!all(namespredictor %in% variatenames)){
                message('Discarding predictor variates not given in metadata.')
                predictor <- predictor[namespredictor %in% variatenames]
            }
            if(all(variatenames %in% namespredictor)){
                stop('All variates are in the predictor')
            }
            if(length(predictor) == 0){
                message('Warning: discarding all predictors, none valid')
                predictor <- NULL
            }
        } else {
            namespredictor <- NULL
        }

        temp <- dimnames(counts)
        if(!is.null(predictor)){
            predictor <- predictor[variatenames]
            predictor[lengths(predictor) == 0] <- TRUE
            ## select subarray of counts corresponding to the predictor values
            counts <- do.call(`[`, c(list(counts), predictor))
            if(is.null(dim(counts))){
                dim(counts) <- length(counts)
                dimnames(counts) <- temp[
                    -which(names(predictor) %in% variatenames)]
            }
        }

        ## Marginalize frequencies
        counts <- apply(counts, predictand, sum)
        if(is.null(dim(counts))){
            dim(counts) <- length(counts)
            dimnames(counts) <- temp[predictand]
        }

        ## Multiplicative factor for alpha parameters, owing to marginalization
        alphas <- alphas / prod(dimensions[c(predictand, namespredictor)])

        ## reduce value of logauxk by maximum, to avoid overflow
        logauxk <- logauxk - max(log(alphas + max(counts)) + logauxk)

        ## Calculate final probabilities for predictands
        temp <- dimnames(counts) # save dimnames, possibly lost in colSums
        counts <- colSums(exp(log(outer(alphas, counts, `+`)) + logauxk))

    ## Reshape array of results
    if(is.null(dim(counts))){
        dim(counts) <- length(counts)
        dimnames(counts) <- temp
    }
    ## Normalize and output the probability distribution as a vector/array
    counts / sum(counts)
    })
}


infer2 <- function(agent, ...) {
    UseMethod("infer2")
}

infer2.agent <- function(
    agent,
    predictand,
    predictor = NULL
){
#### Calculates conditional or unconditional probability for new unit
    with(agent, {
        variatenames <- names(variates)
        dimensions <- lengths(variates)

        ## Check validity of predictands
        predictand <- unlist(predictand)
        if(!all(predictand %in% variatenames)){
            stop('Unknown predictands')
        }

        ## Check if at least one predictor variate is valid
        if(!is.null(predictor)){
            predictor <- as.list(predictor)

            if(!all(namespredictor %in% variatenames)){
                message('Discarding predictor variates not given in metadata.')
                predictor <- predictor[namespredictor %in% variatenames]
            }
            if(all(variatenames %in% namespredictor)){
                stop('All variates are in the predictor')
            }
            if(length(predictor) == 0){
                message('Warning: discarding all predictors, none valid')
                predictor <- NULL
            }
        }
        namespredictor <- names(predictor)


        temp <- dimnames(counts)
        if(!is.null(predictor)){
            predictor <- predictor[variatenames]
            predictor[lengths(predictor) == 0] <- TRUE
            ## select subarray of counts corresponding to the predictor values
            counts <- do.call(`[`, c(list(counts), predictor))
            if(is.null(dim(counts))){
                dim(counts) <- length(counts)
                dimnames(counts) <- temp[
                    -which(names(predictor) %in% variatenames)]
            }
        }

        ## Marginalize frequencies
        counts <- apply(counts, predictand, sum)
        if(is.null(dim(counts))){
            dim(counts) <- length(counts)
            dimnames(counts) <- temp[predictand]
        }

        ## Multiplicative factor for alpha parameters, owing to marginalization
        alphas <- alphas / prod(dimensions[c(predictand, namespredictor)])

        ## reduce value of logauxk by maximum, to avoid overflow
        logauxk <- logauxk - max(log(alphas + max(counts)) + logauxk)

        ## Calculate final probabilities for predictands
        temp <- dimnames(counts) # save dimnames, possibly lost in colSums
        counts <- colSums(exp(log(outer(alphas, counts, `+`)) + logauxk))

    ## Reshape array of results
    if(is.null(dim(counts))){
        dim(counts) <- length(counts)
        dimnames(counts) <- temp
    }
    ## Normalize and output the probability distribution as a vector/array
    counts / sum(counts)
    })
}


rF <- function(agent, ...) {
    UseMethod("rF")
}


rF.agentcompressed <- function(
    n = 1,
    agent,
    predictand,
    predictor = NULL
){
#### Returns a sample of full-population frequency
#### Requires 'extraDistr'
    with(agent, {
        variatenames <- names(variates)
        dimensions <- lengths(variates)

        ## Load alpha parameters from "agent" object
        ## divide by total number of possible joint values
        alphas <- alphas / prod(dimensions)

    ## Select n alpha parameters according to their probabilities
        alphas <- alphas[sample.int(n = length(alphas),
            size = n, replace = TRUE,
            prob = palphas)]

        ## Check validity of predictands
        predictand <- unlist(predictand)
        if(!all(predictand %in% variatenames)){
            stop('Unknown predictands')
        }

        ## Check if at least one predictor variate is valid
        if(!is.null(predictor)){
            predictor <- as.list(predictor)
            namespredictor <- names(predictor)

            if(!all(namespredictor %in% variatenames)){
                message('Discarding predictor variates not given in metadata.')
                predictor <- predictor[namespredictor %in% variatenames]
            }
            if(all(variatenames %in% namespredictor)){
                stop('All variates are in the predictor')
            }
            if(length(predictor) == 0){
                message('Warning: discarding all predictors, none valid')
                predictor <- NULL
            }
        }

        ## select subarray of counts corresponding to the predictor values
        if(!is.null(predictor)){
            keeprows <- apply(uniquedata[, namespredictor, drop = FALSE], 1,
                function(x) all(x == predictor))

            uniquedata <- uniquedata[keeprows, predictand, drop = FALSE]
            counts <- counts[keeprows]
        }

        ## prepare a probability array
        probs <- numeric(prod(dimensions[predictand]))
        dim(probs) <- dimensions[predictand]
        dimnames(probs) <- variates[predictand]

        ## fill count values in probability array
        uniquedata <- as.matrix(uniquedata[, predictand, drop = FALSE])

        for(i in seq_along(counts)){
            datum <- uniquedata[i, , drop = FALSE]
            probs[datum] <- probs[datum] + counts[i]
        }

        ## Multiplicative factor for alpha parameters, owing to marginalization
        excludevars <- which(variatenames %in% c(predictand, names(predictor)))
        alphas <- prod(dimensions[-excludevars]) * alphas

    ## Create n joint-frequency distributions
    ff <- extraDistr::rdirichlet(n, alpha = outer(alphas, c(probs), `+`))
    ## ff <- nimble::rdirch(n, alpha = outer(alphas, c(counts), `+`))
    dim(ff) <- c(n, dim(probs))
    dimnames(ff) <- c(list(sample = NULL), dimnames(probs))
    aperm(a = ff, perm = c(1L + seq_along(predictand), 1))
    })
}


rF.agent <- function(
    n = 1,
    agent,
    predictand,
    predictor = NULL
){
#### Returns a sample of full-population frequency
#### Requires 'extraDistr'
    with(agent, {
        variatenames <- names(variates)
        dimensions <- lengths(variates)

        ## Load alpha parameters from "agent" object
        ## divide by total number of possible joint values
        alphas <- alphas / prod(dimensions)

    ## Select n alpha parameters according to their probabilities
        alphas <- alphas[sample.int(n = length(alphas),
            size = n, replace = TRUE,
            prob = palphas)]

        ## Check validity of predictands
        predictand <- unlist(predictand)
        if(!all(predictand %in% variatenames)){
            stop('Unknown predictands')
        }

        ## Check if at least one predictor variate is valid
        if(!is.null(predictor)){
            predictor <- as.list(predictor)
            namespredictor <- names(predictor)

            if(!all(namespredictor %in% variatenames)){
                message('Discarding predictor variates not given in metadata.')
                predictor <- predictor[namespredictor %in% variatenames]
            }
            if(all(variatenames %in% namespredictor)){
                stop('All variates are in the predictor')
            }
            if(length(predictor) == 0){
                message('Warning: discarding all predictors, none valid')
                predictor <- NULL
            }
        }

        temp <- dimnames(counts)
        if(!is.null(predictor)){
            predictor <- predictor[variatenames]
            predictor[lengths(predictor) == 0] <- TRUE
            ## select subarray of counts corresponding to the predictor values
            counts <- do.call(`[`, c(list(counts), predictor))
            if(is.null(dim(counts))){
                dim(counts) <- length(counts)
                dimnames(counts) <- temp[
                    -which(names(predictor) %in% variatenames)]
            }
        }

        ## Marginalize frequencies
        counts <- apply(counts, predictand, sum)
        if(is.null(dim(counts))){
            dim(counts) <- length(counts)
            dimnames(counts) <- temp[predictand]
        }

        ## Multiplicative factor for alpha parameters, owing to marginalization
        excludevars <- which(variatenames %in% c(predictand, names(predictor)))
        alphas <- prod(dimensions[-excludevars]) * alphas

        ## Create n joint-frequency distributions
    ff <- extraDistr::rdirichlet(n, alpha = outer(alphas, c(counts), `+`))
    ## ff <- nimble::rdirch(n, alpha = outer(alphas, c(counts), `+`))
    dim(ff) <- c(n, dim(counts))
    dimnames(ff) <- c(list(sample = NULL), dimnames(counts))
    aperm(a = ff, perm = c(1L + seq_along(predictand), 1))
    })
}



plotFsamples1D <- function(
    agent,
    n = 100,
    predictand,
    predictor = NULL,
    probability = TRUE,
    file = NULL,
    ...
){
#### Plots samples of full-population freq. distributions for one variate
#### Requires 'png' to plot png
    with(agent, {
        variatenames <- names(variates)
        dimensions <- lengths(variates)

        ## Check validity of predictands
        predictand <- unlist(predictand)
        if(!all(predictand %in% variatenames)){
            stop('Unknown predictands')
        }

        ## Check if at least one predictor variate is valid
        if(!is.null(predictor)){
            predictor <- as.list(predictor)
            namespredictor <- names(predictor)

            if(!all(namespredictor %in% variatenames)){
                message('Discarding predictor variates not given in metadata.')
                predictor <- predictor[namespredictor %in% variatenames]
            }
            if(all(variatenames %in% namespredictor)){
                stop('All variates are in the predictor')
            }
            if(length(predictor) == 0){
                message('Warning: discarding all predictors, none valid')
                predictor <- NULL
            }
        }
        if(length(predictand) > 1){
        stop('This function only work with one predictand.')
        }

        samples <- rF(n = n, agent = agent,
            predictand = predictand, predictor = predictor)

        if(!is.null(file)){
            filext <- sub(".*\\.|.*", "", file, perl=TRUE)
            if(filext == 'pdf'){
                mypdf(sub('.pdf$', '', file))
            } else {
                mypng(sub('.png$', '', file))
            }
        }

        xx <- seq_along(variates[[predictand]])
        tplot(y = samples, x = xx,
            type='b', lty=1, lwd=1, pch=16, col=7, alpha=0.25,
            xticks = xx,
            xlabels = variates[[predictand]],
            xlab = bquote(italic(.(names(dimnames(samples))[1]))),
            ylab = 'probability',
            ...
        )
        if(probability){
            fmean <- infer(agent = agent,
                predictand = predictand, predictor = predictor)
            tplot(y = fmean, x = xx,
                type='b', lty=1, lwd=4, pch=18, col=1, alpha=0.75,
                add = TRUE
            )
        }
        if(!is.null(file)){
            dev.off()
        }
    })
}


decide <- function(
    probs = NULL,
    utils = NULL
){
#### Makes a decision depending on probabilities and utilities
#### Decisions correspond to ROWS of the utilities array

    ## Sanity checks for input arguments
    if(is.null(probs) && is.null(utils)){
        stop("Either 'probs' or 'utils' must be given")
    }

    if(is.null(utils)){ # utilities not given: assume accuracy utility
        if(!is.null(dim(probs)) && length(dim(probs)) > 1){
            stop("'probs' is not a vector, please specify array of utilities")
        }
        utils <- diag(length(probs))

        if(!is.null(dimnames(probs))){
            dimnames(utils) <- c(decision = dimnames(probs),
                dimnames(probs))
        } else {
            if(is.null(names(probs))){
                names(probs) <- 1:length(probs)
            }
            dimnames(utils) <- list(decision = names(probs),
                outcome = names(probs))
        }
    }else if(is.null(probs)){
        ## probabilities not given: assume uniform probs
        probs <- rep(1 / ncol(utils), ncol(utils))
        dim(probs) <- ncol(utils)
        if(is.null(dimnames(utils))){
            dimnames(utils) <- list(decision = 1:nrow(utils),
                outcome = seq_len(ncol(utils)))
        }
        dimnames(probs) <- dimnames(utils)[-1]
    }

    ## Check that dimension of probability variates and utilities match
    if(length(probs) != prod(dim(utils)[-1])){
        stop('Mismatch between inference and utility variates')
    }
    if(is.null(dimnames(utils))){
        if(dim(utils)[1] == dim(utils)[2]){
            dimnames(utils) <- c(decision = dimnames(probs),
                dimnames(probs))
        } else {
            dimnames(utils) <- c(decision = seq_len(nrow(utils)),
                dimnames(probs))
        }
    } else {
        if(!identical(dimnames(utils)[-1], dimnames(probs))){
            stop('Mismatch in outcomes for utilities and probabilities')
        }
        if(is.null(dimnames(utils)[[1]])){
            if(dim(utils)[1] == dim(utils)[2]){
                dimnames(utils)[1] <- dimnames(probs)
            } else {
                dimnames(utils)[1] <- list(decision = seq_len(nrow(utils)))
            }
        }
    }
    decisions <- dimnames(utils)[[1]] # names of decisions
    ##
    ## If necessary, do some array reshaping for faster computation
    if(length(dim(utils)) > 2 || length(dim(probs)) > 1){
        dim(utils) <- c(dim(utils)[1], prod(dim(utils)[-1]))
        rownames(utils) <- decisions
        dim(probs) <- prod(dim(probs))
    }
    ## Calculate expected utilities (matrix product)
    exputils <- sort((utils %*% probs)[, 1], decreasing = TRUE)
    ## Select one decision with max expected utility
    optimal <- names(exputils)[which(exputils == max(exputils))]
    if(length(optimal) > 1){ # if only one requested, then sample
        optimal <- sample(optimal, size = 1)
    }
    ## Output sorted decisions and one optimal decisions
    list(EUs = exputils, optimal = optimal)
}


mutualinfo <- function(
    agent,
    A,
    B,
    base = 2
){
#### Returns the mutual information between two sets of variates A, B
    with(agent, {
        variatenames <- names(variates)

    ## Sanity checks of input arguments
    if(!all(A %in% variatenames)){
        message('Discarding A variates not present in probs')
        A <- A[A %in% variatenames]
    }
    if(!all(B %in% variatenames)){
        message('Discarding B variates not present in probs')
        B <- B[B %in% variatenames]
    }
    if(any(A %in% B)){
        stop('A and B have variates in common.')
    }

        ## calculate the joint probability of variates A and B
        probs <- infer(agent = agent, predictand = c(A, B),
            predictor = NULL)

    ## Calculate marginal joint probability for (A,B) if necessary
    ## probs <- apply(probs, c(A, B), sum)
    ## probs <- probs / sum(probs)
        sum(probs * (log2(probs) -
                         log2(outer(
                             apply(probs, A, sum),
                             apply(probs, B, sum),
                             `*`
                         ))), na.rm = TRUE) / log2(base)
        })
}


