## Colour-blind friendly palettes, from https://personal.sron.nl/~pault/
## palette(colour('bright')())
tolpalette <- c(black='black',
    red='#EE6677',
    blue='#4477AA',
    green='#228833',
    yellow='#CCBB44',
    purple='#AA3377',
    cyan='#66CCEE',
    grey='#BBBBBB',
    midgrey='#888888')
palette(tolpalette)
## names(palette()) <- c('black', 'red', 'blue','green','yellow','purple','cyan','grey','midgrey')
##
## mycolourscheme <- str(structure(c('black', '#445566'), missing = NA, class = c("color_scheme", "color_discrete")))
## cc <- khroma::colour('bright')()
## cc[8] <- '#000000'
## names(cc)[8] <- 'black'
## cc[9] <- '#777777'
## names(cc)[9] <- 'midgrey'
## palette(cc)
## rm(cc)
## bluepurple <- palette()[1]
## red <- palette()[2]
## green <- palette()[3]
## yellow <- palette()[4]
## blue <- palette()[5]
## redpurple <- palette()[6]
## grey <- palette()[7]
## midgrey <- palette()[9]
## darkgrey <- '#555555'
## black <- '#000000'
## scale_colour_discrete <- khroma::scale_colour_bright

## to output in pdf format
mypdf <- function(file = 'Rplot', apaper = 5, portrait = FALSE,
                 height = 148 / 25.4, width = 210 / 25.4, asp = NA, ...) {
  if (is.numeric(apaper)) {
    if (portrait) {
      height <- floor(841 / sqrt(2)^(apaper - 1)) / 25.4
      width <- floor(841 / sqrt(2)^(apaper)) / 25.4
    } else {
      width <- floor(841 / sqrt(2)^(apaper - 1)) / 25.4
      height <- floor(841 / sqrt(2)^(apaper)) / 25.4
    }
  }
  if (!is.na(asp)) {
      if(missing(width)){
          width <- height * asp
      }else{
          height <- width/asp
      }
  }
  pdf(file = paste0(sub('.pdf$', '', file), '.pdf'), paper = 'special',
      height = height, width = width, ...)
}

## to output in svg format
mysvg <- function(file = 'Rplot', apaper = 5, portrait = FALSE,
                 height = 148 / 25.4, width = 210 / 25.4, asp = NA, ...) {
  if (is.numeric(apaper)) {
    if (portrait) {
      height <- floor(841 / sqrt(2)^(apaper - 1)) / 25.4
      width <- floor(841 / sqrt(2)^(apaper)) / 25.4
    } else {
      width <- floor(841 / sqrt(2)^(apaper - 1)) / 25.4
      height <- floor(841 / sqrt(2)^(apaper)) / 25.4
    }
  }
  if (!is.na(asp)) {
      if(missing(width)){
          width <- height * asp
      }else{
          height <- width/asp
      }
  }
  svg(file = paste0(sub('.svg$', '', file), '.svg'),
      height = height, width = width, ...)
}


## to output in png format
mypng <- function(file = 'Rplot', res = 300, apaper = 5, portrait = FALSE,
                 height = 148 / 25.4, width = 210 / 25.4, asp = NA, ...) {
  if (is.numeric(apaper)) {
    if (portrait) {
      height <- floor(841 / sqrt(2)^(apaper - 1)) / 25.4
      width <- floor(841 / sqrt(2)^(apaper)) / 25.4
    } else {
      width <- floor(841 / sqrt(2)^(apaper - 1)) / 25.4
      height <- floor(841 / sqrt(2)^(apaper)) / 25.4
    }
  }
  if (!is.na(asp)) {
      if(missing(width)){
          width <- height * asp
      }else{
          height <- width/asp
      }
  }
  png(file = paste0(sub('.png$', '', file), '.png'),
      height = height, width = width, units = 'in', res = res, ...)
}

## to output in png format
myjpg <- function(file = 'Rplot', res = 300, apaper = 5, portrait = FALSE,
    height = 148 / 25.4, width = 210 / 25.4, asp = NA,
    quality = 90, ...) {
  if (is.numeric(apaper)) {
    if (portrait) {
      height <- floor(841 / sqrt(2)^(apaper - 1)) / 25.4
      width <- floor(841 / sqrt(2)^(apaper)) / 25.4
    } else {
      width <- floor(841 / sqrt(2)^(apaper - 1)) / 25.4
      height <- floor(841 / sqrt(2)^(apaper)) / 25.4
    }
  }
  if (!is.na(asp)) {
      if(missing(width)){
          width <- height * asp
      }else{
          height <- width/asp
      }
  }
  jpeg(file = paste0(sub('.jpg$', '', file), '.jpg'),
      height = height, width = width, units = 'in',
      res = res, quality = quality, ...)
}

myflexiplot <- function(
    x, y,
    xdomain = NULL, ydomain = NULL,
    xlim = NULL, ylim = NULL,
    type = 'l',
    pch = c(1, 0, 2, 5, 6, 3, 4),
    grid = TRUE,
    add = FALSE,
    lwd = 1,
    family = 'Palatino',
    mgp = c(1.5, 0.5, 0),
    oma = c(0.5, 0.5, 0.5, 0.5),
    mar = c(3, 2.5, 1, 1),
    ...
){
    xat <- yat <- NULL

    if(missing('x') && !missing('y')){
        x <- seq_len(NROW(y))
    } else if(!missing('x') && missing('y')){
        y <- seq_len(NROW(x))
    } else if(missing('x') && missing('y')){
        stop('Arguments "x" and "y" cannot both be missing')
    }

    ## if x is character, convert to numeric
    if(is.character(x)){
        if(is.null(xdomain)){ xdomain <- unique(x) }
        ## we assume the user has sorted the vaules in a meaningful order
        ## because the lexical order may not be correct
        ## (think of values like 'low', 'medium', 'high')
        x <- as.numeric(factor(x, levels = xdomain))
        xat <- seq_along(xdomain)
    }

    ## if y is character, convert to numeric
    if(is.character(y)){
        if(is.null(ydomain)){ ydomain <- unique(y) }
        ## we assume the user has sorted the vaules in a meaningful order
        ## because the lexical order may not be correct
        ## (think of values like 'low', 'medium', 'high')
        y <- as.numeric(factor(y, levels = ydomain))
        yat <- seq_along(ydomain)
    }

    ## Syntax of xlim and ylim that allows
    ## for the specification of only upper- or lower-bond
    if(length(xlim) == 2){
        if(is.null(xlim[1]) || !is.finite(xlim[1])){ xlim[1] <- min(x[is.finite(x)]) }
        if(is.null(xlim[2]) || !is.finite(xlim[2])){ xlim[2] <- max(x[is.finite(x)]) }
    }
    if(length(ylim) == 2){
        if(is.null(ylim[1]) || !is.finite(ylim[1])){ ylim[1] <- min(y[is.finite(y)]) }
        if(is.null(ylim[2]) || !is.finite(ylim[2])){ ylim[2] <- max(y[is.finite(y)]) }
    }
    
    par(family = family, mgp = mgp, oma = oma, mar = mar)
    graphics::matplot(x, y, xlim = xlim, ylim = ylim, type = type, pch = pch, axes = F, add = add, lwd = lwd, ...)
    if(!add){
        graphics::axis(1, at = xat, labels = xdomain, lwd = 0, ...)
        graphics::axis(2, at = yat, labels = ydomain, lwd = 0, ...)
        if(grid){
            graphics::grid(nx = NULL, ny = NULL, lty = 1, col = '#BBBBBB80')
        }
    }
}

myplotquantiles <- function(
    x, y,
    xdomain = NULL,
    alpha.f = 0.25,
    col = 9,
    border = NA,
    ...
){
    ## ## TODO: modify so that a vertical plot is also possible
    if(!is.matrix(y) || ncol(y) %% 2 != 0) {
        stop('"y" must be a matrix with an even number of columns.')
    }
    nquant <- ncol(y)

    isfin <- ( (is.numeric(x) & is.finite(x)) | !is.na(x)) &
        apply(y, 1, function(xx){all(is.finite(xx))})
    x <- unname(x[isfin])
    y <- unname(y[isfin, , drop = FALSE])

    ##
    ## col[!grepl('^#',col)] <- palette()[as.numeric(col[!grepl('^#',col)])]
    if(is.na(alpha.f)){alpha.f <- 1}
    col <- adjustcolor(col, alpha.f = alpha.f)
    ## if(is.na(alpha)){alpha <- ''}
    ## else if(!is.character(alpha)){alpha <- alpha2hex(alpha)}
    ## if(!(is.na(col) | nchar(col)>7)){col <- paste0(col, alpha)}
    ##
    myflexiplot(x = x, y = y, xdomain = xdomain, type = 'n', ...)

    ## if x is character, convert to numeric
    if(is.character(x)){
        if(is.null(xdomain)){ xdomain <- unique(x) }
        ## we assume the user has sorted the vaules in a meaningful order
        ## because the lexical order may not be correct
        ## (think of values like 'low', 'medium', 'high')
        x <- as.numeric(factor(x, levels = xdomain))
    }

    for(ii in seq_len(nquant/2)) {
        graphics::polygon(x=c(x, rev(x)), y=c(y[,ii], rev(y[, nquant + 1 - ii])),
            col = col, border = border)
    }
}


mytplot <- function(x, y, xlim = c(NA, NA), ylim = c(NA, NA), asp = NA,
    n = 10, family = '', xticks = NULL, xlabels = TRUE,
    yticks = NULL, ylabels = TRUE, cex = 1.5, ly = NULL,
    lx = NULL, mar = NULL, lty.axis = 1, lwd.axis = 0,
    lwd.ticks = 1, col.ticks = '#bbbbbb80', col.lab = 'black',
    cex.axis = 1.12, las.y = 1, xgrid = NULL, ygrid = NULL,
    main = NULL, cex.main = 1.5, xlab = NULL, ylab = NULL,
    cex.lab = 1.5, type = 'l', col = palette(),
    pch = c(1, 0, 2, 5, 6, 3, 4), lty = 1:4, lwd = 2, alpha = NA,
    border = palette(), border.alpha = NA, xtransf = NULL,
    ytransf = NULL, add = FALSE) {
    ## palette(khroma::colour('bright')())
    ## scale_colour_discrete <- khroma::scale_colour_bright
    ## if (missing(x)) {
    ##     if (missing(y))
    ##         stop("must specify at least one of 'x' and 'y'")
    ##     else x <- seq_len(NROW(y))
    ## }
    ## else if (missing(y)) {
    ##     if (missing(x))
    ##         stop("must specify at least one of 'x' and 'y'")
    ##     else y <- seq_len(NROW(x))
    ## }
    if (!missing(y) && !missing(x)) {
        if (!is.list(x)) {
            x <- apply(cbind(x), 2, identity, simplify = 'list')
        }
        if (!is.list(y)) {
            y <- apply(cbind(y), 2, identity, simplify = 'list')
        }
    }
    ##
    else if (missing(x) && !missing(y)) {
        if (!is.list(y)) {
            y <- apply(cbind(y), 2, identity, simplify = 'list')
        }
        x <- lapply(y, seq_along)
    } else if (missing(y) && !missing(x)) {
        if (!is.list(x)) {
            x <- apply(cbind(x), 2, identity, simplify = 'list')
        }
        y <- lapply(x, seq_along)
    }
    ##
    xx <- unlist(x)
    yy <- unlist(y)
    if (!is.character(xx)) {
        temp <- unique(xx[is.finite(xx)])
        if(length(temp) > 1) {
            xlim0 <- range(temp)
        } else if(length(temp) == 1){
            xlim0 <- range(temp) + c(-1, 1)
        } else {
            xlim0 <- c(0, 1)
        }
    } else {
        uxx <- unique(xx)
        if (is.character(xlabels) && all(uxx %in% xlabels)) {
            uxx <- intersect(xlabels, uxx)
        }
        if (any(type == 'h')) {
            xlim0 <- c(0.5, length(uxx) + 0.5)
        } else {
            xlim0 <- c(1, length(uxx))
        }
    }
    if (!is.character(yy)) {
        temp <- unique(yy[is.finite(yy)])
        if(length(temp) > 1) {
            ylim0 <- range(temp)
        } else if(length(temp) == 1){
            ylim0 <- range(temp) + c(-1, 1)
        } else {
            ylim0 <- c(0, 1)
        }
    } else {
        uyy <- unique(yy)
        if (is.character(ylabels) && all(uyy %in% ylabels)) {
            uyy <- intersect(ylabels, uyy)
        }
        if (any(type == 'h')) {
            ylim0 <- c(0.5, length(uyy) + 0.5)
        } else {
            ylim0 <- c(1, length(uyy))
        }
    }
    if (is.na(ylim[1]) & any(type == 'h')) {
        ylim[1] <- 0
    }
    xlim[is.na(xlim)] <- xlim0[is.na(xlim)]
    ylim[is.na(ylim)] <- ylim0[is.na(ylim)]
    if (length(n) < 2) {
        n <- rep(n, 2)
    }
    if (is.null(xticks)) {
        if (!is.character(xx)) {
            xticks <- pretty(xlim, n = n[1])
        } else {
            xticks <- seq_along(uxx)
        }
    }
    if (is.character(xx) && length(xlabels) == 1 && xlabels == TRUE) {
        xlabels <- uxx
    }
    ## if(length(xlabels)==1 && xlabels){
    ##     xlabels <- xticks
    ##     xlabels[!(xticks %in% pretty(xlim, n=round(n[1]/2)))] <- NA
    ## }
    if (is.null(yticks)) {
        if (!is.character(yy)) {
            yticks <- pretty(ylim, n = n[2])
        } else {
            yticks <- seq_along(uyy)
        }
    }
    if (is.character(yy) && length(ylabels) == 1 && ylabels == TRUE) {
        ylabels <- uyy
    }
    ## if(length(ylabels)==1 && ylabels){
    ##     ylabels <- yticks
    ##     ylabels[!(yticks %in% pretty(ylim, n=round(n[2]/2)))] <- NA
    ## }
    if (is.null(lx)) {
        lx <- 0
    }
    if (is.null(ly)) {
        ly <- 1
        if (!(length(yticks) == 1 && (any(is.na(yticks) | yticks == FALSE)))) {
            ly <- 1 + max(nchar(sprintf('%.7g', yticks))) * 0.75
        } else if (length(ylabels) > 1) {
            ly <- 1 + max(nchar(ylabels)) * 0.75
        }
    }
    if (is.null(xlab)) {
        xlab <- names(x)[1]
    }
    if (is.null(ylab)) {
        ylab <- names(y)[1]
    }
    ##
    if (!add) {
        plot.new()
        ## par(mai=c(2, 3.5, 2, 0)/2.54, family='Palatino')#, mar=c(4,6,4,0)+0.1)
        if (is.null(main)) {
            marup <- 0
        } else {
            marup <- 3.5
        }
        if (is.null(mar)) {
            mar <- c(3.25, ly, marup, 1) + c(1, 1.5, 1, 1)
        }
        mar[is.na(mar)] <- (c(3.25, ly, marup, 1) + c(1, 1.5, 1, 1))[is.na(mar)]
        par(mar = mar, family = family) # , mar=c(4,6,4,0)+0.1)
        ##
        plot.window(xlim = xlim, ylim = ylim, xaxs = 'r', yaxs = 'r', asp = asp)
        ##
        if (!is.null(xtransf)) {
            xlabels <- xtransf(xticks)
        }
        if (!is.null(ytransf)) {
            ylabels <- ytransf(yticks)
        }
        if (!(length(xticks) == 1 & (any(is.na(xticks) | xticks == FALSE)))) {
            axis(side = 1, at = xticks, labels = xlabels, tick = TRUE, lty = lty.axis,
                lwd = lwd.axis, lwd.ticks = lwd.ticks, col.ticks = col.ticks,
                gap.axis = NA, cex.axis = cex.axis, line = 0)
        }
        if (!(length(yticks) == 1 & (any(is.na(yticks) | yticks == FALSE)))) {
            axis(side = 2, at = yticks, labels = ylabels, tick = TRUE, lty = lty.axis,
                lwd = lwd.axis, lwd.ticks = lwd.ticks, col.ticks = col.ticks,
                gap.axis = NA,las = las.y, cex.axis = cex.axis, line = 0)
        }
        ##
        if (length(cex.lab) == 1) {
            cex.lab <- rep(cex.lab, 2)
        }
        if (length(col.lab) == 1) {
            col.lab <- rep(col.lab, 2)
        }
        if (!is.null(main)) {
            title(main = main, cex.main = cex.main, line = 3)
        }
        if (!is.null(xlab)) {
            title(xlab = xlab, cex.lab = cex.lab[1], line = 3 + lx,
                col.lab = col.lab[1])
        }
        if (!is.null(ylab)) {
            title(ylab = ylab, cex.lab = cex.lab[2], line = ly, col.lab = col.lab[2])
        }
    }
    if (is.null(xgrid)) {
        xgrid <- !add
    }
    if (is.null(ygrid)) {
        ygrid <- !add
    }
    if (xgrid) {
        for (i in xticks) {
            abline(v = i, lty = lty.axis, lwd = lwd.ticks, col = col.ticks)
        }
    }
    if (ygrid) {
        for (i in yticks) {
            abline(h = i, lty = lty.axis, lwd = lwd.ticks, col = col.ticks)
        }
    }
    ##
    ## col[!grepl('^#',col)] <- palette()[as.numeric(col[!grepl('^#',col)])]
    ## border[!grepl('^#',border)] <- palette()[as.numeric(border[!grepl('^#',border)])]
    if (is.numeric(col)) {
        col <- palette()[col]
    }
    if (is.numeric(border)) {
        col <- palette()[border]
    }
    ##
    nx <- length(x)
    ny <- length(y)
    if (all(!is.na(x) & !is.na(y))) {
        for (j in 1:max(nx, ny)) {
            xx <- x[[(j - 1) %% nx + 1]]
            if (is.character(xx)) {
                xx <- match(xx, uxx)
            }
            yy <- y[[(j - 1) %% ny + 1]]
            if (is.character(yy)) {
                yy <- match(yy, uyy)
            }
            if (length(xx) > length(yy) + 1 || length(yy) > length(xx) + 1) {
                stop(paste0('plot ', j, ': "x" and "y" must have same number ',
                    'of rows or differ by 1'))
            }
            ialpha <- alpha[(j - 1) %% length(alpha) + 1]
            icol <- col[(j - 1) %% length(col) + 1]
            if (!(type[(j - 1) %% length(type) + 1] == 'h' ||
                      length(xx) == length(yy) + 1 ||
                      length(yy) == length(xx) + 1)) { # not a histogram
                if (is.na(ialpha)) {
                    ialpha <- 1
                }
                icol <- adjustcolor(icol, alpha.f = ialpha)
                ## else if(!is.character(ialpha)){ialpha <- alpha2hex(ialpha)}
                ## if(!(is.na(icol) | nchar(icol)>7)){icol <- paste0(icol, ialpha)}
                ##
                plot.xy(xy.coords(x = xx, y = yy),
                    type = type[(j - 1) %% length(type) + 1],
                    col = icol,
                    pch = pch[[(j - 1) %% length(pch) + 1]],
                    lty = lty[(j - 1) %% length(lty) + 1],
                    lwd = lwd[(j - 1) %% length(lwd) + 1],
                    cex = cex[(j - 1) %% length(cex) + 1]
                )
            } else { # histogram
                iborder <- border[(j - 1) %% length(border) + 1]
                iborder.alpha <- border.alpha[(j - 1) %% length(border.alpha) + 1]
                ##
                if (is.na(ialpha)) {
                    ialpha <- 0.5
                }
                icol <- adjustcolor(icol, alpha.f = ialpha)
                ##
                if (is.na(iborder.alpha)) {
                    iborder.alpha <- 0.5
                }
                iborder <- adjustcolor(iborder, alpha.f = iborder.alpha)
                ##
                if (length(yy) == length(xx)) {
                    xx <- c(
                    (3 * xx[1] - xx[2]) / 2,
                    xx[-length(xx)] + diff(xx) / 2,
                    (3 * xx[length(xx)] - xx[length(xx) - 1]) / 2
                    )
                }
                if (length(xx) == length(yy) + 1) {
                    ## if(is.na(ialpha)){ialpha <- '80'}
                    ## else if(!is.character(ialpha)){ialpha <- alpha2hex(ialpha)}
                    ## if(!(is.na(icol) | nchar(icol)>7)){icol <- paste0(icol, ialpha)}
                    ## if(is.na(iborder.alpha)){iborder.alpha <- '80'}
                    ## else if(!is.character(iborder.alpha)){iborder.alpha <- alpha2hex(iborder.alpha)}
                    ## if(!(is.na(iborder) | nchar(iborder)>7)){iborder <- paste0(iborder, iborder.alpha)}
                    for (i in 1:(length(xx) - 1)) {
                        polygon(
                            x = rbind(xx[i], xx[i], xx[i + 1], xx[i + 1]),
                            y = rbind(ylim[1], yy[i], yy[i], ylim[1]),
                            col = icol, border = iborder,
                            lty = lty[(j - 1) %% length(lty) + 1],
                            lwd = lwd[(j - 1) %% length(lwd) + 1]
                        )
                    }
                } else {
                    ## iborder <- border[(j-1)%%length(border)+1]
                    ## iborder.alpha <- border.alpha[(j-1)%%length(border.alpha)+1]
                    ##
                    ## if(is.na(ialpha)){ialpha <- 0.5}
                    ## icol <- alpha2hex(icol, ialpha)
                    ## if(is.na(ialpha)){ialpha <- '80'}
                    ## else if(!is.character(ialpha)){ialpha <- alpha2hex(ialpha)}
                    ## if(!(is.na(icol) | nchar(icol)>7)){icol <- paste0(icol, ialpha)}
                    ##
                    ## if(is.na(iborder.alpha)){iborder.alpha <- 0.5}
                    ## iborder <- alpha2hex(iborder, iborder.alpha)
                    ## if(is.na(iborder.alpha)){iborder.alpha <- '80'}
                    ## else if(!is.character(iborder.alpha)){iborder.alpha <- alpha2hex(iborder.alpha)}
                    ## if(!(is.na(iborder) | nchar(iborder)>7)){iborder <- paste0(iborder, iborder.alpha)}
                    for (i in 1:(length(yy) - 1)) {
                        polygon(
                            y = rbind(yy[i], yy[i], yy[i + 1], yy[i + 1]),
                            x = rbind(xlim[1], xx[i], xx[i], xlim[1]),
                            col = icol, border = iborder,
                            lty = lty[(j - 1) %% length(lty) + 1],
                            lwd = lwd[(j - 1) %% length(lwd) + 1]
                        )
                    }
                }
            }
        }
    }
}

mylegend <- function(x, y=NULL, legend, col=palette(), pch=c(1,0,2,5,6,3,4), lty=1:4, lwd=2, alpha=1, cex=1.5, ...){
    suppressWarnings(col <- mapply(function(i,j)adjustcolor(i,j),col,alpha))
    legend(x=x, y=y, legend=legend, col=col, pch=pch, lty=lty, lwd=lwd, bty='n', cex=cex, ...)
}

myfivenumaxis <- function(side, x, col='#555555', type=6){
    x <- x[!is.na(x) && is.finite(x)]
    if(length(x)==0){x <- c(0,1)}
    if(diff(range(x))==0){x <- range(x) + c(-1,1)}
    ylim <- par('usr')
    five <- c(min(x), quantile(x=x, probs=(1:3)/4, type=type), max(x))
    ##
    if(side==1){
        xl <- rbind(five[c(1,4)], five[c(2,5)])
        yl <- rep(ylim[3],2)
        xp <- five[3]
        yp <- ylim[3]
    }else if(side==2){
        yl <- rbind(five[c(1,4)], five[c(2,5)])
        xl <- rep(ylim[1],2)
        yp <- five[3]
        xp <- ylim[1]
    }else if(side==3){
        xl <- rbind(five[c(1,4)], five[c(2,5)])
        yl <- rep(ylim[2],2)
        xp <- five[3]
        yp <- ylim[2]
    }else if(side==4){
        yl <- rbind(five[c(1,4)], five[c(2,5)])
        xl <- rep(ylim[4],2)
        yp <- five[3]
        xp <- ylim[4]
    }
    ##
    matlines(x=xl, y=yl, lty=1, lwd=2, col=col)
    matpoints(x=xp, y=yp, pch=18, cex=2, col=col)
}

## scatteraxis(): use rug()

myhist <- function(x, n=NULL, type=6, pretty=FALSE, plot=FALSE,
    extendbreaks=FALSE, ylim=c(0,NA), ...){
    if(!is.list(x)){x <- list(x)}
    if(!is.list(n)){n <- list(n)}
    out <- list()
    for(i in 1:length(x)){
        ax <- x[[i]]
        an <- n[[(i-1)%%length(n)+1]]
        if(is.character(ax)){
            nextout <- c(table(ax[!is.na(ax)]))
            nextout <- list(
                breaks=NA,
                counts=unname(nextout),
                density=unname(nextout)/sum(nextout),
                mids=names(nextout),
                xname=names(x)[i],
                equidist=NA
            )
        }else{
        ax <- ax[!is.na(ax) & is.finite(ax)]
        if(is.null(an)){an <- (round(sqrt(length(ax))/2))}
    if(length(an)==1 && (is.na(an) || an=='i' || an=='integer')){breaks <- (round(min(ax))-0.5):(round(max(ax))+0.5)}
    else if(length(an) > 1 || is.character(an)){breaks <- an}
    else if(length(an) == 1 && an > 0){
        rg <- range(ax)
        if(diff(rg)==0){rg <- rg + c(-0.5,0.5)}
        breaks <- seq(rg[1], rg[2], length.out=an+1)}
    else if(length(an) == 1 && an < 0){
        rg <- range(ax)
        if(diff(rg)==0){rg <- rg + c(-0.5,0.5)}
        breaks <- seq(rg[1], rg[2]-an, by=-an)
        breaks <- breaks - (breaks[length(breaks)]-rg[2])/2
    }
    else {print('Error with n')}
        if(!is.null(pretty) && pretty){
            breaks <- pretty(ax, n=length(breaks)-1)
        }
        if(extendbreaks){
         breaks <- c(-Inf,breaks,+Inf)
        }
        nextout <- hist(x=ax, breaks=breaks, plot=FALSE)
        }
        out <- c(out,list(nextout))
    }
    if(plot){
        mytplot(x=lapply(out,function(xx){
            if(length(xx$breaks)==1){xx$mids}else{xx$breaks}
        } ),
              y=lapply(out,function(xx)xx$density),ylim=ylim,type='h', ...)
    }else{
        if(length(out)==1){unlist(out,recursive=F)}else{out}
    }
}

myquantile <- function(x, probs=c(0.055, 0.25, 0.5, 0.75, 0.945), na.rm=TRUE, names=TRUE, type=6, ...){
    quantile(x=x, probs=probs, na.rm=na.rm, names=names, type=type, ...)
}

mymad <- function(x){mad(x, constant=1, na.rm=TRUE)}

mysummary <- function(x){
    x <- cbind(x)
    apply(x, 2, function(xx){
        c(quantile(xx, c(0.055, 0.25, 0.5, 0.75, 0.945), type=6, na.rm=TRUE), MAD=mad(xx,constant=1,na.rm=T), IQR=IQR(xx,na.rm=T), mean=mean(xx,na.rm=T), sd=sd(xx,na.rm=T), hr=diff(range(xx,na.rm=T))/2, min=min(xx,na.rm=T), max=max(xx,na.rm=T), NAs=sum(is.na(xx)))
    })
}

## Function to build powerset
mypowerset <- function(set){
  n <- length(set)
  masks <- 2^(1:n-1)
  lapply( 1:2^n-1, function(u) set[ bitwAnd(u, masks) != 0 ] )
}

## Greatest common denominator
mygcd <- function(...){Reduce(function(a, b){if (b == 0) a else Recall(b, a %% b)}, c(...))}

## Normalize according to row
mynormalize <- function(x){
    if(is.null(dim(x)) || is.table(x)){
        x/sum(x,na.rm=T)
    }else{
        aperm(aperm(x)/c(aperm(cbind(colSums(x,na.rm=T)))))
    }
}

## Table with list of values
mytable <- function(x, values=NULL, norm=FALSE){
    if(norm){
        mynormalize(table(c(x,values))-!(is.null(values)))
    }else{
        table(c(x,values))-!(is.null(values))
        }
}

mybisect <- function(fn, a, b){
    if(fn(a) * fn(b) > 0){
        stop('Cannot use bisection.')
    } else if(fn(a) > 0){
        m <- b
        b <- a
        a <- m
    }
    m <- (a + b)/2
    while(abs(a - b) > .Machine$double.eps &&
              abs(fn(m)) > .Machine$double.eps){
                  if(fn(m) > 0){
                      b <- m
                  } else {
                      a <- m
                  }
                  m <- (a + b)/2
              }
    list(value = m, objective = fn(m))
}

myread.csv <- function(file, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", na.strings = '', stringsAsFactors = FALSE, tryLogical = FALSE, ...){
    read.csv(file, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", na.strings = '', stringsAsFactors = FALSE, tryLogical = FALSE, ...)
}
