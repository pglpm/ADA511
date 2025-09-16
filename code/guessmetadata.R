guessmetadata <- function(
    data,
    file = NULL
){
#### Guess metadata information from dataset and save it in a metadata file
    if(is.character(data)){
        if(is.null(file)){
            file <- paste0('meta_', data)
        }
        data <- read.csv(data,
            na.strings = '', stringsAsFactors = FALSE, tryLogical = FALSE)
    }else{
        if(is.null(file)){
            cat("\n'file' argument missing: output to stdout\n")
        }
    }
    ##
    nvariates <- ncol(data)
    nn <- sapply(data, function(xx){length(unique(xx))})
    maxN <- max(nn)
    ## str(matrix(NA, nrow=nvariates,ncol=2+maxN, dimnames=list(NULL, c('variate','domainsize',paste0('V',1:maxN)))))
    metadata <- as.data.frame(matrix(character(),
        nrow = nvariates, ncol = 2 + maxN,
        dimnames = list(NULL,
            c('variate', 'domainsize', paste0('V', 1:maxN)))
    ))
    ## print(metadata)
    metadata[['variate']] <- colnames(data)
    metadata[['domainsize']] <- nn
    for(i in 1:nvariates){
        metadata[i, paste0('V', 1:nn[i])] <- as.list(sort(unique(data[[i]])))
    }
    ##
    if(!is.null(file)){
        write.csv(x = metadata, file = file,
            row.names = FALSE, quote = FALSE, na = '')
    }else{
        metadata
    }
}
