findP <- function(x, ...) {
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
        E <- matrix(1, nn + na, na)
        F <- numeric(nn + na)
        E[nn + (1:na), ] <- diag(na)
        D <- c(rep('==', nn), rep('>=', na))
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
        E <- matrix(1, nn + na + 2, na + 1)
        F <- numeric(nn + na + 2)
        E[nn, na + 1] <- -1
        E[nn + 1, ] <- c(
            1L * apply(combos, 1, function(zz){
                eval(Tcond, as.list(zz))
            }),
            0)
        E[nn + 1 + (1:(na+1)), ] <- diag(na + 1)
        F[nn + 1] <- 1
        D <- c(rep('==', nn + 1), rep('>=', na + 1))
    }
    F[nn] <- 1
    ##
    ##
    Tp <- substitute(alist(...))
    for(i in 2:nn) {
        j <- i - 1
        left <- Tp[[i]][[2]]
        right <- Tp[[i]][[3]]
        ##
        if(!is.numeric(try(eval(right), silent = TRUE))) {
            ## equality between two probabilities
            if(length(right) == 2) {
                temp <- right
                right <- substitute(a * 1)
                right[[2]] <- temp
            }
            if(!(deparse(right[[1]]) %in% c('*', '/'))) {
                stop('invalid right side in argument ', i)
            }
            coeff <- eval(right[[1]])(1, right[[3]])
            right <- right[[2]]
            ##
            Esuppl <- left[[2]]
            Esuppr <- right[[2]]
            if(
            !(length(Esuppl) == 1 ||
                 !(deparse(Esuppl[[1]]) == '~')) ||
                !(length(Esuppr) == 1 ||
                     !(deparse(Esuppr[[1]]) == '~'))
            ) {
                ## both probabilities have conditional
                if(!(Esuppl[[3]] == Esuppr[[3]])) {
                    stop('invalid conditionals in argument ', i)
                }
                Esuppl[[1]] <- `&&`
                Esuppr[[1]] <- `&&`
            }
            ##
            E[j, ] <- c(
                1L * apply(combos, 1, function(zz){
                    eval(Esuppl, as.list(zz))
                }) - coeff * apply(combos, 1, function(zz){
                    eval(Esuppr, as.list(zz))
                }),
                extraE)
        } else {
            ## right side is numeric
            coeff <- eval(right)
            ##
            Esupp <- left[[2]]
            if(length(Esupp) == 1 ||
                   !(deparse(Esupp[[1]]) == '~')) {
                ## no conditional
                E[j, ] <- c(
                    1L * apply(combos, 1, function(zz){
                        eval(Esupp, as.list(zz))
                    }) - coeff,
                    extraE)
            } else {
                ## conditional
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
    }
    ##
    ##
    ## print(list(E = E, F = F, EqA = EqA))
    minp <- lpSolve::lp(
        direction = 'min',
        objective.in = EqA,
        const.mat = E,
        const.dir = D,
        const.rhs = F
    )
    maxp <- lpSolve::lp(
        direction = 'max',
        objective.in = EqA,
        const.mat = E,
        const.dir = D,
        const.rhs = F
    )
    c(min = minp$objval, max = maxp$objval)
}
