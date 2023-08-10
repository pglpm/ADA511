fknowledge <- function(data, metadata){
    if(is.character(data)){
        data <- fread(data, na.strings='')
    }
    if(is.character(metadata)){
        metadata <- fread(metadata, na.strings='')
    }
    ##
    variates <- metadata$variate
    nvariates <- nrow(metadata)
    rgvariates <- metadata$N
    names(rgvariates) <- variates
    alphas <- array(1/prod(rgvariates), dim=rgvariates,
                    dimnames=apply(mdatat2,1,function(xx){unname(xx[paste0('V',1:(xx['N']))])}, simplify=list))
    ##
    if(!(is.null(data) || all(is.na(data)))){
        for(arow in 1:nrow(data)){
            temp <- rbind(as.character(data[arow,..variates]))
        alphas[temp] <- alphas[temp] +1
    }
    }
    if(is.null(dim(alphas))){
        tempnames <- names(alphas)
        dim(alphas) <- length(alphas)
        dimnames(alphas) <- list(tempnames)
    }
    attr(alphas, 'variates') <- metadata$variate
    attr(alphas, 'N') <- metadata$N
    alphas
}

fmarginal <- function(fknowledge, variates){
    temp <- apply(fknowledge, match(variates, attr(fknowledge,'variates')),
                  sum, na.rm=T)
    if(is.null(dim(temp))){
        tempnames <- names(temp)
        dim(temp) <- length(temp)
        dimnames(temp) <- list(tempnames)
    }
    temp
}

fconditional <- function(fknowledge, conditional){
    conditional <- rbind(conditional)
    ncond <- match(colnames(conditional), attr(fknowledge,'variates'))
    totake <- as.list(rep(TRUE, length(dim(fknowledge))))
    totake[ncond] <- conditional
    alphas <- do.call('[', c(list(fknowledge), totake))
    if(is.null(dim(alphas))){
        tempnames <- names(alphas)
        dim(alphas) <- length(alphas)
        dimnames(alphas) <- list(tempnames)
    }
    attr(alphas, 'variates') <- attr(fknowledge,'variates')[-ncond]
    attr(alphas, 'N') <- attr(fknowledge,'N')[-ncond]
    alphas
}


fmean <- function(fknowledge){fknowledge/sum(fknowledge)}

fsamples <- function(n, fknowledge){
    array(extraDistr::rdirichlet(n, alpha=c(fknowledge)),
                  dim=c(n, dim(fknowledge)),
                  dimnames=c(list(NULL), dimnames(fknowledge))
                  )
}

fquantiles <- function(p, fknowledge){
    alpha0 <- sum(fknowledge)
    temp <- apply(fknowledge, 1:length(dim(fknowledge)),
                  function(xx){qbeta(p=p, shape1=xx, shape2=alpha0-xx)})
    dimnames(temp)[[1]] <- paste0(p*100,'%')
    temp
}
