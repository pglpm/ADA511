#### Helper function: separate punctation from words
separatepunct <- function(text){
    text <- toupper(text)
    text <- gsub(
        "[^1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ,.;:?!%$&']",
        "", text)
    text <- gsub("[0-9]+", "@", text)
    text <- unlist(
        strsplit(x = gsub(
            "([,.;:?!#])",
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
    values <- rep(tokens, each = n)
    dim(values) <- c(n, length(tokens))
    colnames(values) <- paste0('V', seq_along(tokens))
    metadata <- cbind(metadata, values)

    metafile <- paste0('meta-', sub('.txt$', '', outsuffix), '.csv')
    write.csv(x = metadata, file = metafile,
        row.names = FALSE, quote = TRUE, na = '')

    message('Saved files.')
    c(metafile, ngramfile)
}


