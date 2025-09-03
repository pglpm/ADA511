findP <- function(x, ...) {
    ## number of constraints
    nn <- length(substitute(alist(...)))
    ## find atomic sentences and prepare truth table for DNF
    tvals <- c(FALSE, TRUE)
    atoms <- sort(all.vars(as.formula(substitute(~ alist(x, ...)))))
    ttable <- list()
    for(i in atoms) {
        ttable[[i]] <- tvals
    }
    combos <- expand.grid(ttable)
    ## total number of conjunctions
    na <- 2L^length(atoms)
    ## print(combos) # for debugging

### Target probability
    Tp <- substitute(x)
    ##
    if(length(Tp[[2]]) == 1 ||
           !(deparse(Tp[[2]][[1]]) == '~')) {
        ## it doesn't have a conditional
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
        ## it does have a conditional
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

### Constraint probabilities, analysed one at a time
    Tp <- substitute(alist(...))

    for(i in 2:nn) {
        j <- i - 1
        left <- Tp[[i]][[2]]
        right <- Tp[[i]][[3]]

        if(!is.numeric(try(eval(right), silent = TRUE))) {
            ## constraint is equality between two probabilities
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

            Esuppl <- left[[2]]
            Esuppr <- right[[2]]
            if(
            !(length(Esuppl) == 1 ||
                 !(deparse(Esuppl[[1]]) == '~')) ||
                !(length(Esuppr) == 1 ||
                     !(deparse(Esuppr[[1]]) == '~'))
            ) {
                ## both probabilities have conditional
                ## we use the rule of cond. prob.
                if(!(Esuppl[[3]] == Esuppr[[3]])) {
                    stop('invalid conditionals in argument ', i)
                }
                Esuppl[[1]] <- `&&`
                Esuppr[[1]] <- `&&`
            }

            E[j, ] <- c(
                1L * apply(combos, 1, function(zz){
                    eval(Esuppl, as.list(zz))
                }) - coeff * apply(combos, 1, function(zz){
                    eval(Esuppr, as.list(zz))
                }),
                extraE)
        } else {
            ## constraint is numeric
            coeff <- eval(right)

            Esupp <- left[[2]]
            if(length(Esupp) == 1 ||
                   !(deparse(Esupp[[1]]) == '~')) {
                ## probability has no conditional
                E[j, ] <- c(
                    1L * apply(combos, 1, function(zz){
                        eval(Esupp, as.list(zz))
                    }) - coeff,
                    extraE)
            } else {
                ## probability has conditional
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

    ## print(list(E = E, F = F, EqA = EqA)) # for debugging
    ## Find minimum value
    minp <- lpSolve::lp(
        direction = 'min',
        objective.in = EqA,
        const.mat = E,
        const.dir = D,
        const.rhs = F
    )
    ## Find maximum value
    maxp <- lpSolve::lp(
        direction = 'max',
        objective.in = EqA,
        const.mat = E,
        const.dir = D,
        const.rhs = F
    )

    c(min = minp$objval, max = maxp$objval)
}
