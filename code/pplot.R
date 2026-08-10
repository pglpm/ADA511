#' Plot numeric or character values
#'
#' @description
#' Plot function that modifies and expands the **graphics** package's [graphics::matplot()] function in several ways.
#'
#' @details
#' This function is essentially a wrapper around [graphics::matplot()], augmenting the latter with some features useful for plotting data and probabilities handled by **Prova**. Some of the additional features provided by `pplot` are the following:
#'
#' - Either or both `x` and `y` arguments can be [list][base::list()]s. In this case, the first element of `x` is plotted against the first element of `y`, and so on, recycling as necessary. This allows for plots having different numbers of base points. The specifications in arguments like `type`, `lty`, `col`, `alpha.f`, `xjitter`, and similar apply to each list element in turn.
#' - Argument `x`, or each element in `x` if it is a list, can be of class [`base::character`]. In this case, x-axis labels as given in `xdomain` are used, or the unique values in `x` if `xdomain` is `NULL`. Similarly for `y` and `ydomain`. This feature makes it easier to plot nominal and ordinal non-numeric variates.
#' - Additional plot `type`s are available: `'hx'`, `'qx'`, `'hy'`, `'qy'` (internally they use [graphics::polygon()]):
#'   - `'hx'` plots shaded histograms. Argument `x` must be a list of `breaks`, and `y` a list of `counts` or `densities`, for example produced by by [graphics::hist()].
#'   - `'qx'` plots shaded bands. The first band extends from the line defined by the first column of `y`, to the line defined by the *last* column; the second band is similarly delimited by the second and second-last columns of `y`, and so on (if `y` has an odd number of columns, the central one defines a line rather than a band). The x-values are the corresponding columns of `x`, recycled if necessary. This plot `type` is useful for plotting quantile bands calculated with [Pr()].
#'   - `'hy'`, `'qy'` are analogous `type`s, but with the roles of `x` and `y` switched.
#' - A jitter can be added to each plot, via the `xjitter` and `yjitter` vectors of switches. When either of these arguments is `NA`, it is internally assessed whether jitter is necessary. This feature makes it easier to generate scatter plots of nominal, ordinal, or rounded-continuous variates.
#' - It is possible to specify only a lower or upper limit in the `xlim` and `ylim` arguments, letting the other limit to be found automatically. This feature is useful in plotting probabilities and histograms, when we want to specify the lower as `0` but want the upper limit to be the the maximum probability.
#' - Transparency of lines or markers can be specified through argument `alpha.f`.
#' - Some defaults are different from [base::plot()] and [graphics::matplot()].
#'
#' See the package's vignettes for more examples.
#'
#' @param x Numeric or character or list: vectors of x-coordinates. If an element of `x` is missing, a numeric vector `1:...` is created having as many values as the rows of the corresponding element in `y`.
#' @param y Numeric or character or list: vectors of y-coordinates. If an element of `y` is missing, a numeric vector `1:...` is created having as many values as the rows of the corresponding element in `x`.
#' @param type Character vector or list indicating the type of plot for each element of `x` and `y`. The types of plot are the same as in [base::plot()], in particular `'p'` for points, `'l'` for lines, `'b'` for both points and lines, `'c'` for empty points joined by lines, `'o'` for overplotted points and lines, ``n'` for empty plot. Additional special types `'hx'`, `'qx'`, `'hy'`, `'qy'` are available for plotting histograms and quantile bands; see "Details".
#' @param xdomain,ydomain Character or numeric or `NULL` (default): vector of possible values of the variates represented in the `x`- and `y`-axes, in case the `x` or `y` argument is a character vector. Note that the domains apply to all elements in `x` and `y`. The ordering of the values is respected. If `NULL`, then `unique(x)` or `unique(y)` is used.
#' @param xlim,ylim `NULL` (default) or a vector of two values. If non-`NULL` and any of the two values is not finite (including `NA` or `NULL`), then the `min` or `max` `x`- or `y`-coordinates of the plotted points are used.
#' @param alpha.f Numeric vector or list, default `1`: opacity of the line or contour colours, `0` being completely invisible and `1` completely opaque.
#' @param xjitter,yjitter Vector or list of logicals or `NA` (default): add [base::jitter()] to `x`- or `y`-values? Useful when plotting discrete variates. If `NA`, jitter is added if both `x` and `y` are of character (or factor) class.
#' @param fill Logical or `NA` (default). For histogram plots (`type = 'hx'` or `'hy'`), value `TRUE` means fill the histogram, and do not plot its contour; `FAlSE` means plot only its contour without filling; `NA` means plot contour and fill. For quantile plots (`type = 'qx'` or `'qy'`), value `TRUE` do not plot the bands' contours; `FAlSE` means plot only the contours without filling; `NA` plots a contour only when the quantile band has zero area (and would be invisible otherwise).
#' @param alpha.f.fill Numeric vector or list, default `0.25`: opacity of the filling colours, `0` being completely invisible and `1` completely opaque.
#' @param grid Logical, default `TRUE`: plot a light grid?
#' @param lwd.grid Numeric, default 1: width of grid lines.
#' @param col.grid Color of grid lines, default `'#00000022'`. Can be specified in any of the usual ways, see for instance [grDevices::col2rgb()].
#' @param lty,lwd,pch,lend,col,xlab,ylab,add,axes,cex.main see analogous arguments in [graphics::matplot()] and [graphics::plot.default()]; defaults are different (see "Usage").
#' @param ... Other parameters to be passed to [graphics::matplot()].
#'
#' @return `NULL`, [invisibly][base::invisible()]; produces a plot, see [graphics::matplot()].
#'
#' @examples
#' ## Scatter plot of 'island' vs 'species' variates of the 'penguins' dataset;
#' ## note how jitter is automatically added:
#' pplot(x = penguins[, 'species'], y = penguins[, 'island'])
#'
#'
#' ## Scatter plot of 'bill_len' vs 'species':
#' pplot(x = penguins[, 'species'], y = penguins[, 'bill_len'])
#'
#' ## Scatter plot of 'bill_len' vs 'body_mass';
#' ## in this case the scatter-plot `type = 'p'` must be specified:
#' pplot(x = penguins[, 'body_mass'], y = penguins[, 'bill_len'], type = 'p')
#'
#' ## Plot y-values having different numbers of x-values
#' pplot(x = list(1:5, 6:7), y = list(5:1, 6:7))
#'
#' ## Specify only the minimum plotting range
#' xgrid <- seq(from = -2, to = 2, length.out = 65)
#' pplot(x = xgrid, y = dnorm(xgrid), ylim = c(0, NA))
#'
#' ## Draw a shaded histogram
#' histo <- hist(rnorm(1000), breaks = 'FD', plot = FALSE)
#' pplot(x = histo$breaks, y = histo$density, type = 'hx')
#'
#' @import grDevices
#' @import graphics
#'
#' @concept display
#' @export
pplot <- function(
    x = NULL, y = NULL,
    type = NA,
    lty = c(1, 2, 4, 3, 6, 5),
    lwd = 2,
    lend = par('lend'),
    pch = c(1, 2, 0, 5, 6, 3), #, 4,
    col = palette(),
    xlab = NA, ylab = NA,
    xlim = NULL, ylim = NULL,
    add = FALSE,
    xdomain = NULL, ydomain = NULL,
    alpha.f = 1,
    xjitter = NA,
    yjitter = NA,
    fill = NA,
    alpha.f.fill = 0.25,
    grid = TRUE,
    lwd.grid = NULL,
    col.grid = '#00000022',
    axes = FALSE,
    cex.main = 1,
    ...
){
    if(!is.list(x)){x <- list(x)}
    if(!is.list(y)){y <- list(y)}

    ## Elements: unlist, unfactor, other fixes
    for(aplot in seq_along(x)){
        if(is.list(x[[aplot]])){x[[aplot]] <- unlist(x[[aplot]],
            recursive = TRUE, use.names = TRUE)}
        if(is.factor(x[[aplot]])){x[[aplot]] <- as.character(x[[aplot]])}
        if(is.na(type[[(aplot - 1) %% length(type) + 1]]) &&
               NROW(x[[aplot]]) == NROW(y[[(aplot - 1) %% length(y) + 1]]) + 1){
            type[[(aplot - 1) %% length(type) + 1]] <- 'hx'
        }
        if(is.character(x[[aplot]]) &&
               (anyDuplicated(x[[aplot]]) || length(x[[aplot]]) == 1)){
            if(is.na(xjitter[[(aplot - 1) %% length(xjitter) + 1]])){
                xjitter[[(aplot - 1) %% length(xjitter) + 1]] <- TRUE
            }
            if(is.na(type[[(aplot - 1) %% length(type) + 1]])){
                type[[(aplot - 1) %% length(type) + 1]] <- 'p'
            }
        }
    }

    for(aplot in seq_along(y)){
        if(is.list(y[[aplot]])){y[[aplot]] <- unlist(y[[aplot]],
            recursive = TRUE, use.names = TRUE)}
        if(is.factor(y[[aplot]])){y[[aplot]] <- as.character(y[[aplot]])}
        if(is.na(type[[(aplot - 1) %% length(type) + 1]]) &&
               NROW(y[[aplot]]) == NROW(x[[(aplot - 1) %% length(x) + 1]]) + 1){
            type[[(aplot - 1) %% length(type) + 1]] <- 'hy'
        }
        if(is.character(y[[aplot]]) &&
               (anyDuplicated(y[[aplot]]) || length(y[[aplot]]) == 1)){
            if(is.na(yjitter[[(aplot - 1) %% length(yjitter) + 1]])){
                yjitter[[(aplot - 1) %% length(yjitter) + 1]] <- TRUE
            }
            if(is.na(type[[(aplot - 1) %% length(type) + 1]])){
                type[[(aplot - 1) %% length(type) + 1]] <- 'p'
            }
        }
    }

    if(!is.null(xdomain)){ xdomain <- unlist(xdomain) }
    if(!is.null(ydomain)){ ydomain <- unlist(ydomain) }

    ## Find NULL elements for special handling later
    xnull <- vapply(X = x, FUN = is.null, FUN.VALUE = FALSE, USE.NAMES = FALSE)
    ynull <- vapply(X = y, FUN = is.null, FUN.VALUE = FALSE, USE.NAMES = FALSE)

    ## Handle special case of NA: 1D ensemble plot
    if(length(x) == 1 && is.na(x)){
        x <- 0
        if(is.null(xdomain)){ xdomain <- NA }
        if(is.na(type)){ type <- 'p' }
    }
    if(length(y) == 1 && is.na(y)){
        y <- 0
        if(is.null(ydomain)){ ydomain <- NA }
        if(is.na(type)){ type <- 'p' }
    }

    ## Check consistency of x, y args; find ranges
    if(all(xnull)){
        xcha <- FALSE
        rgx <- c(Inf, -Inf)
    } else if(all(vapply(X = x[!xnull], FUN = is.numeric,
        FUN.VALUE = FALSE, USE.NAMES = FALSE))){
        ## all x are numeric, find common min max
        xcha <- FALSE
        rgx <- unlist(x, recursive = FALSE, use.names = FALSE)
        rgx <- range(rgx[is.finite(rgx)])
    } else if(all(vapply(X = x[!xnull], FUN = is.character,
        FUN.VALUE = FALSE, USE.NAMES = FALSE))){
        ## all x are character, find domain
        xcha <- TRUE
        if(is.null(xdomain)){
            xdomain <- unlist(x, recursive = FALSE, use.names = FALSE)
            xdomain <- unique(xdomain[!is.na(xdomain)])
        }
        rgx <- c(1, length(xdomain))
    } else {
        stop("Elements in 'x' must be all numeric or all character.")
    }

    if(all(ynull)){
        ycha <- FALSE
        rgy <- c(Inf, -Inf)
    } else if(all(vapply(X = y[!ynull], FUN = is.numeric,
        FUN.VALUE = FALSE, USE.NAMES = FALSE))){
        ## all y are numeric, find common min max
        ycha <- FALSE
        rgy <- unlist(y, recursive = FALSE, use.names = FALSE)
        rgy <- range(rgy[is.finite(rgy)])
    } else if(all(vapply(X = y[!ynull], FUN = is.character,
        FUN.VALUE = FALSE, USE.NAMES = FALSE))){
        ## all y are character, find domain
        ycha <- TRUE
        if(is.null(ydomain)){
            ydomain <- unlist(y, recursive = FALSE, use.names = FALSE)
            ydomain <- unique(ydomain[!is.na(ydomain)])
        }
        rgy <- c(1, length(ydomain))
    } else {
        stop("Elements in 'y' must be all numeric or all character.")
    }

    ## Handle NULLs
    if(any(xnull)){
        temp <- lapply(
            X = y[ rep(x = seq_along(y), length.out = length(x))[xnull] ],
            FUN = function(xx){seq_len(NROW(xx))}
        )
        x[xnull] <- temp
        rgx[1] <- min(rgx[1], unlist(temp), na.rm = TRUE)
        rgx[2] <- max(rgx[2], unlist(temp), na.rm = TRUE)
        rm(temp)
    }
    if(any(ynull)){
        temp <- lapply(
            X = x[ rep(x = seq_along(x), length.out = length(y))[ynull] ],
            FUN = function(xx){seq_len(NROW(xx))}
        )
        y[ynull] <- temp
        rgy[1] <- min(rgy[1], unlist(temp), na.rm = TRUE)
        rgy[2] <- max(rgy[2], unlist(temp), na.rm = TRUE)
        rm(temp)
    }

    ## Other NAs
    type[is.na(type)] <- 'l'
    xjitter[is.na(xjitter)] <- FALSE
    yjitter[is.na(yjitter)] <- FALSE

    ## Plot ranges
    if(!isTRUE(is.finite(xlim[1]))){
        if(any(xjitter)){ rgx[1] <- rgx[1] - 1/3 }
        if(any(type == 'hy')){ rgx[1] <- min(rgx[1], 0) }
        xlim[1] <- min(rgx)
    }
    if(!isTRUE(is.finite(xlim[2]))){
        if(any(xjitter)){ rgx[2] <- rgx[2] + 1/3 }
        xlim[2] <- max(rgx)
    }
    if(xlim[1] == xlim[2]){
        xlim[1] <- xlim[1] - 1/3
        xlim[2] <- xlim[2] + 1/3
    }

    if(!isTRUE(is.finite(ylim[1]))){
        if(any(yjitter)){ rgy[1] <- rgy[1] - 1/3 }
        if(any(type == 'hx')){ rgy[1] <- min(rgy[1], 0) }
        ylim[1] <- min(rgy)
    }
    if(!isTRUE(is.finite(ylim[2]))){
        if(any(yjitter)){ rgy[2] <- rgy[2] + 1/3 }
        ylim[2] <- max(rgy)
    }
    if(ylim[1] == ylim[2]){
        ylim[1] <- ylim[1] - 1/3
        ylim[2] <- ylim[2] + 1/3
    }

    ## Parameters for q-type plots

    ## Function for preparing indices for q-type plots
    qindices <- function(groups, col1 = 1, col2 = 1){
        indices <- cumsum(groups$lengths)
        n <- length(indices)
        col1 <- indices[n] * (col1 - 1)
        col2 <- indices[n] * (col2 - 1)
        unlist(mapply(
            FUN = function(v, i1, i2){
                if(v){c(col1 + (i1:i2), col2 + (i2:i1))}else{NA}
            },
            groups$values,
            c(1, 1 + indices[-n]),
            indices,
            USE.NAMES = FALSE, SIMPLIFY = FALSE
        ))
    }

    ## First plot window
    graphics::matplot(x = NA, y = NA, type = 'n',
        xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim,
        cex.main = cex.main, add = add, axes = FALSE,
        ## xaxs = 'i', yaxs = 'i',
        ...)

### Plot the lists
    for(aplot in seq_len(max(length(x), length(y), na.rm = FALSE))){
        ## ## drop unneeded dimensions?
        ## thisx <- drop(x[[aplot]])
        ## thisy <- drop(y[[aplot]])
        thisx <- x[[(aplot - 1) %% length(x) + 1]]
        thisy <- y[[(aplot - 1) %% length(y) + 1]]
        thistype <- type[[(aplot - 1) %% length(type) + 1]]

        ## If only one coordinate is given, it's meant for all points
        ## need duplicating in order to have different jitters
        if(length(thisy) == 1 && NROW(thisx) > 1){
            thisy <- rep.int(x = thisy, times = NROW(thisx))
            if(is.na(thistype)){ thistype <- 'p' }
        } else if(length(thisx) == 1 && NROW(thisy) > 1){
            thisx <- rep.int(x = thisx, times = NROW(thisy))
            if(is.na(thistype)){ thistype <- 'p' }
        }

        ## convert characters to integers, according to domains
        if(xcha){
            temp <- dim(thisx)
            thisx <- as.numeric(factor(thisx, levels = xdomain))
            dim(thisx) <- temp
        }
        if(ycha){
            temp <- dim(thisy)
            thisy <- as.numeric(factor(thisy, levels = ydomain))
            dim(thisy) <- temp
        }

        thiscol <- col[[(aplot - 1) %% length(col) + 1]]
        thisalpha.f <- alpha.f[[(aplot - 1) %% length(alpha.f) + 1]]

        thisfill <- fill[[(aplot - 1) %% length(fill) + 1]]
        thisalpha.f.fill <-
            alpha.f.fill[[(aplot - 1) %% length(alpha.f.fill) + 1]]

        ## Check if jitter needed
        thisxjitter <- xjitter[[(aplot - 1) %% length(xjitter) + 1]]
        if((is.na(thisxjitter) && anyDuplicated(thisx)) || isTRUE(thisxjitter)){
            ## handle different jitter logic for one-number-only
            if(any(thisx != thisx[1])){
                thisx <- jitter(thisx, factor = 5/3)
            } else {
                thisx <- jitter(thisx, amount = 1/3)
            }
        }
        thisyjitter <- yjitter[[(aplot - 1) %% length(yjitter) + 1]]
        if((is.na(thisyjitter) && anyDuplicated(thisy)) || isTRUE(thisyjitter)){
            ## handle different jitter logic for one-number-only
            if(any(thisy != thisy[1])){
                thisy <- jitter(thisy, factor = 5/3)
            } else {
                thisy <- jitter(thisy, amount = 1/3)
            }
        }

        ## Plot
        ## checks for type = 'q'
        thistype <- type[[(aplot - 1) %% length(type) + 1]]

        if(thistype %in% c('hx', 'hy')){
            if(thistype == 'hx'){
                thisx <- rep(x = thisx, each = 4)
                thisx <- thisx[-c(1, length(thisx))]
                thisy <- c(0, rep(x = thisy, each = 4), 0)
            } else if(thistype == 'hy'){
                thisy <- rep(x = thisy, each = 4)
                thisy <- thisy[-c(1, length(thisy))]
                thisx <- c(0, rep(x = thisx, each = 4), 0)
            }

            if(is.na(thisfill) || thisfill){
                graphics::polygon(
                    x = thisx, y = thisy,
                    border = NA,
                    col = adjustcolor(thiscol, alpha.f = thisalpha.f.fill),
                    lwd = lwd[[(aplot - 1) %% length(lwd) + 1]],
                    density = NULL, xpd = TRUE, lty = 1)
            }
            if(is.na(thisfill) || !thisfill){ thistype <- 'l' }
        }

        if(!(thistype %in% c('qx', 'qy', 'hx', 'hy'))){

            ## Plot
            graphics::matplot(x = thisx, y = thisy,
                type = thistype,
                lty = lty[[(aplot - 1) %% length(lty) + 1]],
                lwd = lwd[[(aplot - 1) %% length(lwd) + 1]],
                lend = lend[[(aplot - 1) %% length(lend) + 1]],
                pch = pch[[(aplot - 1) %% length(pch) + 1]],
                col = adjustcolor(thiscol, alpha.f = thisalpha.f),
                add = TRUE, ...)

        }
        if(thistype %in% c('qx', 'qy')){

            if(thistype == 'qx'){
                if(is.null(dim(thisy))){ dim(thisy) <- c(1, length(thisy)) }
                nquant <- ncol(thisy)
                groups <- rle(!is.na(c(thisx)))

                ## quantiles of zero x-extension need a border
                if(is.na(thisfill)){
                    border <- rep.int(x = NA, times = length(thisx))
                    border[(groups$length[groups$values] == 1)] <-
                        adjustcolor(thiscol, alpha.f = thisalpha.f.fill)
                }

                for(ii in seq_len(ceiling(nquant / 2))){
                    graphics::polygon(
                        x = thisx[qindices(groups = groups,
                            col1 = 1, col2 = 1)],
                        y = thisy[qindices(groups = groups,
                            col1 = ii, col2 = nquant + 1 - ii)],
                        ## x = c(thisx[,(ii - 1) %% temp + 1],
                        ##     rev(thisx[,(ii - 1) %% temp + 1])),
                        ## y = c(thisy[, ii], rev(thisy[, nquant + 1 - ii])),
                        border = border,
                        col = adjustcolor(thiscol, alpha.f = thisalpha.f.fill),
                        lwd = lwd[[(aplot - 1) %% length(lwd) + 1]],
                        density = NULL, xpd = TRUE, lty = 1)
                }
            } else {
                if(is.null(dim(thisx))){ dim(thisx) <- c(1, length(thisx)) }
                nquant <- ncol(thisx)
                groups <- rle(!is.na(c(thisy)))

                ## quantiles of zero y-extension need a border
                if(is.na(thisfill)){
                    border <- rep.int(x = NA, times = length(thisy))
                    border[(groups$length[groups$values] == 1)] <-
                        adjustcolor(thiscol, alpha.f = thisalpha.f.fill)
                }

                for(ii in seq_len(ceiling(nquant / 2))){
                    graphics::polygon(
                        y = thisy[qindices(groups = groups,
                            col1 = 1, col2 = 1)],
                        x = thisx[qindices(groups = groups,
                            col1 = ii, col2 = nquant + 1 - ii)],
                        ## x = c(thisx[,(ii - 1) %% temp + 1],
                        ##     rev(thisx[,(ii - 1) %% temp + 1])),
                        ## y = c(thisy[, ii], rev(thisy[, nquant + 1 - ii])),
                        border = border,
                        col = adjustcolor(thiscol, alpha.f = thisalpha.f.fill),
                        lwd = lwd[[(aplot - 1) %% length(lwd) + 1]],
                        density = NULL, xpd = TRUE, lty = 1)
                }
            }
        }
    }

    xat <- yat <- xaxp <- yaxp <- NULL

    if(xcha){
        xat <- seq_along(xdomain)
        if(any(xjitter)){
            xaxp <- c(range(xat) + c(-0.5, 0.5), length(xat))
        } else {
            xaxp <- c(range(xat), length(xat) - 1)
        }
    }
    if(ycha){
        yat <- seq_along(ydomain)
        if(any(yjitter)){
            yaxp <- c(range(yat) + c(-0.5, 0.5), length(yat))
        } else {
            yaxp <- c(range(yat), length(yat) - 1)
        }
    }

    ## Final axes
    if(!add || axes){
        graphics::axis(side = 1, at = xat, labels = xdomain, tick = axes,
            col = 'black', lwd = 1, lty = 1, ...)
        graphics::axis(side = 2, at = yat, labels = ydomain, tick = axes,
            col = 'black', lwd = 1, lty = 1, ...)
    }

    ## Final grid
    if(grid){
        ## Save and restore user's par()
        if(!is.null('xaxp')){
            oldparx <- par(xaxp = xaxp)
            on.exit(par(oldparx))
        }
        if(!is.null('yaxp')){
            oldpary <- par(yaxp = yaxp)
            on.exit(par(oldpary), add = TRUE)
        }
        graphics::grid(nx = NULL, ny = NULL, lty = 1,
            lwd = lwd.grid, col = col.grid)
    }
    invisible()
}
