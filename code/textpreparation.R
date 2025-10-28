#### Helper function: separate punctation from words
separatepunct <- function(text){
    text <- toupper(text)
    text <- gsub(
        "[^1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ,.;:?!%$&']",
        "", text)
    text <- gsub("[0-9]+", " n ", text)
    text <- unlist(
        strsplit(x = gsub(
            "([,.;:?!n])",
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
combinetokens <- function(token1, token2){
    if(substr(token2, 1, 1) %in%
           c(",", ".", ";", ":", "?", "!", "'")
    ){
        paste0(token1, token2)
    } else {
        paste0(token1, ' ', token2)
    }
}


#### Save files with n-grams and metadata
preparengramfiles <- function(
    inputfile,
    outsuffix = inputfile,
    n = 3,
    ntokens = 500
) {
    ## retrieve text from file and do a first word division
    text <-  separatepunct(scan(file = inputfile,
        what = 'character', sep = "", quote= NULL, allowEscapes = FALSE))

    ## find unique tokens
    tokens <- sort(table(text), decreasing = TRUE)
    message('Unique tokens: ', length(tokens), '.')

    ## restrict number of unique tokens, discarding the less common ones
    tokens <- names(tokens)[seq_len(min(ntokens, length(tokens)))]

    ## flag tokens not containing the vocabulary
    text[!(text %in% tokens)] <- NA

    ## build n-grams
    text <- buildngram(text = text, n = n)
    colnames(text) <- paste0('word', seq_len(n))

    ## remove ngrams with missing entries
    text <-text[complete.cases(text), , drop = FALSE]

    ## save n-gram data file
    ngramfile <- paste0('ngram-', sub('.txt$', '', outsuffix), '.csv')
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

    metafile <- paste0('meta-', sub('.txt$', '', outsuffix), '.csv')
    write.csv(x = metadata, file = metafile,
        row.names = FALSE, quote = TRUE, na = '')

    message('Saved files.')
    c(metafile, ngramfile)
}

#### Generate text of a given length
generatetext <- function(
    agent,
    length = 100,
    start = NULL,
    online = TRUE
) {
    with(agent, {
        n <- length(variates)

        ## check that start has words in vocabulary
        notwords <- start[!(start %in% variates[[1]])]
        if(length(notwords) > 0){
            stop('Tokens: ', paste0(notwords, collapse = ' '),
                ' not in vocabulary')
        }

        ## outtext will contain the whole generated text
        outtext <- '\n'
        predictor <- start[length(start) - ((n - 2L):0L)]
        n0 <- length(predictor)
        if(!is.null(predictor)){
            predictor <- as.list(predictor)
            names(predictor) <- paste0('word', seq_len(n0))
        }

        for(i in seq_along(start)){
            outtext <- combinetokens(outtext, start[i])
            }

        for(i in seq_len(n - 1L - n0)){
            wordi <- paste0('word', n0 + i)
            out <- infer(agent = agent,
                predictand = wordi,
                predictor = predictor)
            nextw <- sample(x = names(out), size = 1, prob = out)
            ## nextw <- names(out)[sample(which(out == max(out)), 1)]
            predictor <- c(predictor, setNames(list(nextw), wordi))
            wordi <- paste0('word', n0 + i + 1L)
            outtext <- combinetokens(outtext, nextw)
        }
        ##
        wcount <- n - 1L
        wordn <- paste0('word', n)
        while(wcount < length){
            ## print('***')
            ## print(predor)
            ## print(wordi)
            out <- infer(agent = agent,
                predictand = wordn,
                predictor = predictor)
            nextw <- sample(x = names(out), size = 1, prob = out)
            ## cat(combinetokens(NULL, nextw))
            outtext <- combinetokens(outtext, nextw)
            ##
            predictor[] <- c(predictor[-1], list(nextw))
            ## w2 <- sample(names(out)[which(out == max(out))], 1)
            ##
            wcount <- wcount + 1L
        }
        paste0(outtext, '\n')
    })
}
