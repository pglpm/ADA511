guessmetadata <- function(data, file=NULL){
#### Guess metadata information from dataset and save it in a metadata file
#### Requires 'data.table'
    if(is.character(data)){
            if(is.null(file)){
                file <- paste0('meta_',data)
            }
            data <- fread(data, na.strings='', header=TRUE)
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
    metadata <- as.data.table(matrix(character(),
                                     nrow=nvariates, ncol=2+maxN,
                                     dimnames=list(NULL,
                                                   c('variate', 'domainsize', paste0('V',1:maxN)))
                                     ))
        ## print(metadata)
        metadata[['variate']] <- colnames(data)
        metadata[['domainsize']] <- nn
        for(i in 1:nvariates){
            metadata[i, paste0('V',1:nn[i]) := as.list(sort(unique(data[[i]])))]
        }
        ##
        if(!is.null(file)){
            fwrite(x=metadata, file=file)
        }else{
            metadata
        }
}
