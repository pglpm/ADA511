boundP <- function(x, ...) {
    nn <- length(substitute(alist(...)))
    tvals <- c(FALSE, TRUE)
    atoms <- sort(all.vars(as.formula(substitute(~ alist(x, ...)))))
    na <- 2L^length(atoms)
    ttable <- list()
    for(i in atoms) {
        ttable[[i]] <- tvals
    }
    combos <- expand.grid(ttable)
    ## print(combos)
    ##
    Tp <- substitute(x)
    ##
    if(length(Tp[[2]]) == 1 ||
           !(deparse(Tp[[2]][[1]]) == '~')) {
        extraE <- NULL
        Tsupp <- Tp[[2]]
        EqA <- 1L * apply(combos, 1, function(zz){
            eval(Tsupp, as.list(zz))
        })
        E <- matrix(1, nn, na)
        F <- numeric(nn)
    } else {
        extraE <- 0
        Tsupp <- Tp[[2]]
        Tsupp[[1]] <- `&&`
        Tcond <- Tsupp[[3]]
        EqA <- c(
            1L * apply(combos, 1, function(zz){
                eval(Tsupp, as.list(zz))
            }),
            0)
        E <- matrix(1, nn + 1, na + 1)
        F <- numeric(nn + 1)
        E[nn, na + 1] <- -1
        E[nn + 1, ] <- c(
            1L * apply(combos, 1, function(zz){
                eval(Tcond, as.list(zz))
            }),
            0)
        F[nn + 1] <- 1
    }
    F[nn] <- 1
    ##
    Tp <- substitute(alist(...))
    for(i in 2:nn) {
        j <- i - 1
        coeff <- eval(Tp[[i]][[3]])
        ##
        if(length(Tp[[i]][[2]][[2]]) == 1 ||
            !(deparse(Tp[[i]][[2]][[2]][[1]]) == '~')) {
            Esupp <- Tp[[i]][[2]][[2]]
            E[j, ] <- c(
                1L * apply(combos, 1, function(zz){
                    eval(Esupp, as.list(zz))
                }) - coeff,
                extraE)
        } else {
            Esupp <- Tp[[i]][[2]][[2]]
            Esupp[[1]] <- `&&`
            Econd <- Esupp[[3]]
            E[j, ] <- c(
                1L * apply(combos, 1, function(zz){
                eval(Esupp, as.list(zz))
                }) -
                    coeff * apply(combos, 1, function(zz){
                    eval(Econd, as.list(zz))
                    }),
                extraE)
        }
    }
    ##
    ##
    ## print(list(E = E, F = F, EqA = EqA))
    limSolve::varranges(
        E = E, F = F,
        ispos = TRUE,
        ## G = diag(na + ncond),
        ## H = rep(0, na + ncond),
        EqA = EqA, EqB = 0,
        tol = 1e-100
    )
}

boundP(
    p(c1 ~ y1 && h2),
    p(c1 || c2 || c3) == 1,
    p(c1 && c2) == 0,
    p(c1 && c3) == 0,
    p(c2 && c3) == 0,
    p(h1 && y1) == 0,
    p(h2 && y2) == 0,
    p(h3 && y3) == 0,
    p(h1 && c1) == 0,
    p(h2 && c2) == 0,
    p(h3 && c3) == 0,
    p(h1 && h2) == 0,
    p(h1 && h3) == 0,
    p(h2 && h3) == 0,
    p(h1 || h2 || h3) == 1,
    p(h2 ~ c1 && y1) == 0.5,
    p(h3 ~ c1 && y1) == 0.5,
    p(c1) == 1/3,
    p(c2) == 1/3,
    p(c3) == 1/3,
    p(c1 ~ y1) == 1/3,
    p(c2 ~ y1) == 1/3,
    p(c3 ~ y1) == 1/3
 )

boundP(
    p(c1 ~ y1 & h2),
    p(c1 | c2 | c3) == 1,
    p(c1 & c2) == 0,
    p(c1 & c3) == 0,
    p(c2 & c3) == 0,
    p(h1 & y1) == 0,
    p(h2 & y2) == 0,
    p(h3 & y3) == 0,
    p(h1 & c1) == 0,
    p(h2 & c2) == 0,
    p(h3 & c3) == 0,
    p(h1 & h2) == 0,
    p(h1 & h3) == 0,
    p(h2 & h3) == 0,
    p(h1 | h2 | h3) == 1,
    p(h2 ~ c1 & y1) == 0.5,
    p(h3 ~ c1 & y1) == 0.5,
    p(c1) == 1/3,
    p(c2) == 1/3,
    p(c3) == 1/3,
    p(c1 ~ y1) == 1/3,
    p(c2 ~ y1) == 1/3,
    p(c3 ~ y1) == 1/3
 )

boundP(
    p(x ~ a && b),
    p(x ~ a) == 0.5,
    p(x ~ b) == 0.3
)

boundP(
    p(x ~ a & b),
    p(x ~ a) == 0.5,
    p(x ~ b) == 0.3
)

