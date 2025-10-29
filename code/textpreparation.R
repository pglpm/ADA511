#### Helper function: separate punctation from words
separatepunct <- function(text){
    text <- toupper(iconv(text, to = 'ASCII//TRANSLIT'))
    text <- gsub(
        "[^1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ,.;:?!%$&@+'/-]",
        "", text)
    text <- gsub("[0-9]+", "n", text)
    text <- unlist(
        strsplit(x = gsub(
            "([,.;:?!])",
            " \\1", text), split = " "),
        use.names = FALSE)
    text[text != ""]
}


#### Helper function: build n-grams
buildngram <- function(
    text,
    n = 3
){
    n <- as.integer(n - 1)
    seqn <- 0:n
    t(sapply(seq_len(length(text) - n), function(i){ text[i + seqn] }))
}


#### Helper function: combine words with spaces in the right places
combinetokens <- function(token1, token2, combine = `c`){
    if(substr(token2, 1, 1) %in%
           c(",", ".", ";", ":", "?", "!", "'")
    ){
        combine(token1, token2)
    } else {
        combine(token1, ' ', token2)
    }
}

#### Helper function: add space when necessary
addspace <- function(token){
    if(substr(token, 1, 1) %in%
           c(",", ".", ";", ":", "?", "!", "'")
    ){
        token
    } else {
        paste0(' ', token)
    }
}

#### Helper function: wrap output text
wrapprint <- function(text, wrapat = 60){
    out <- NULL
    width <- 0
    for(token in c(text, ' ...')){
        width <- width + nchar(token)
        if(width > wrapat && substr(token, 1, 1) == ' '){
            out <- c(out, '\n', token)
            width <- 0
        } else {
            out <- c(out, token)
        }
    }
    message(out)
}


#### Save files with n-grams and metadata
preparengramfiles <- function(
    inputfile,
    outsuffix = inputfile,
    n = 3,
    maxtokens = Inf,
    outprefix = NULL
) {
    ## retrieve text from file and do a first word division
    text <-  separatepunct(scan(file = inputfile,
        what = 'character', sep = "", quote= NULL, allowEscapes = FALSE))

    ## find unique tokens
    tokens <- sort(table(text), decreasing = TRUE)
    message('Unique tokens:  ', length(tokens), '.')

    ## restrict number of unique tokens, discarding the less common ones
    tokens <- names(tokens)[seq_len(min(maxtokens, length(tokens)))]

    ## flag tokens not containing the vocabulary
    text[!(text %in% tokens)] <- NA

    ## build n-grams
    text <- buildngram(text = text, n = n)
    colnames(text) <- paste0('word', seq_len(n))

    ## remove ngrams with missing entries
    text <-text[complete.cases(text), , drop = FALSE]
    message('Data:  ', nrow(text), '  ', n ,'-grams.')

    ## save n-gram data file
    ngramfile <- paste0(outprefix, 'ngram-',
        sub('.txt$', '', outsuffix), '.csv')
    write.csv(file = ngramfile, x = text,
        row.names = FALSE, quote = TRUE)

    ## prepare and save metadata file
    metadata <- data.frame(
        variate = paste0('word', seq_len(n)),
        domainsize = rep(length(tokens), n)
    )
    values <- t(matrix(tokens,
        nrow = length(tokens), ncol = n,
        dimnames = list(paste0('V', seq_along(tokens)), NULL)
    ))
    metadata <- cbind(metadata, values)

    metafile <- paste0(outprefix, 'meta-',
        sub('.txt$', '', outsuffix), '.csv')
    write.csv(x = metadata, file = metafile,
        row.names = FALSE, quote = TRUE, na = '')

    message('Files saved.')
    list(metadata = metafile, data = ngramfile)
}

#### Generate text of a given length or up to a given token
generatetext <- function(
    agent,
    stopat = 100,
    prompt = NULL,
    online = FALSE
) {
    with(agent, {
        n <- length(variates)

        ## check that prompt has words in vocabulary
        notwords <- prompt[!(prompt %in% variates[[1]])]
        if(length(notwords) > 0){
            stop('Tokens: ', paste0(notwords, collapse = ' '),
                ' not in vocabulary')
        }

        if(is.numeric(stopat)){
            stoplength <- abs(stopat)
            stopword <- ''
        } else {
            if(!(stopat %in% variates[[1]])){
                stop('Token ', stopat, ' not in vocabulary')
            }
            stoplength <- Inf
            stopword <- stopat
        }

        ## outtext will contain the whole generated text
        outtext <- NULL
        if(online){cat('\n')}
        predictor <- prompt[length(prompt) - ((n - 2L):0L)]
        n0 <- length(predictor)
        if(!is.null(predictor)){
            predictor <- as.list(predictor)
            names(predictor) <- paste0('word', seq_len(n0))
        }

        for(i in seq_along(prompt)){
            nextw <- prompt[i]
            outtext <- c(outtext, addspace(nextw))
            if(online){cat(addspace(nextw))}
        }

        for(i in seq_len(n - 1L - n0)){
            wordi <- paste0('word', n0 + i)
            out <- infer(agent = agent,
                predictand = wordi,
                predictor = predictor)
            nextw <- sample(x = names(out), size = 1, prob = out)

            outtext <- c(outtext, addspace(nextw))
            if(online){cat(addspace(nextw))}

            predictor <- c(predictor, setNames(list(nextw), wordi))
        }
        ##
        wcount <- n - 1L
        wordn <- paste0('word', n)
        while(wcount < stoplength && nextw != stopword){
            ## print('***')
            ## print(predor)
            ## print(wordi)
            out <- infer(agent = agent,
                predictand = wordn,
                predictor = predictor)
            nextw <- sample(x = names(out), size = 1, prob = out)

            outtext <- c(outtext, addspace(nextw))
            if(online){cat(addspace(nextw))}

            predictor[] <- c(predictor[-1], list(nextw))

            wcount <- wcount + 1L
        }
        if(online){cat('...\n')}

        outtext
    })
}
