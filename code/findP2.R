findP2 <- function(x, ...) {
    ## number of constraints
    nc <- length(substitute(alist(...)))
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
    Psyms <- c('P', 'p', 'Pr', 'pr', 'T', 't')

### Target probability
    Tsupp <- substitute(x)
    if(
        length(Tsupp) < 2 || length(Tsupp) > 3 ||
            !(deparse(Tsupp[[1]]) %in% Psyms)
    ) {
        stop('invalid first argument')
    }
    ##
    if(length(Tsupp) == 2) {
        ## it doesn't have a conditional
        extraE <- NULL
        Tsupp <- Tsupp[[2]]
        EqA <- 1L * apply(combos, 1, function(zz){
            eval(Tsupp, as.list(zz))
        })
        E <- matrix(1, nc + na, na)
        F <- numeric(nc + na)
        F[1] <- 1
        E[nc + (1:na), ] <- diag(na)
        D <- c(rep('==', nc), rep('>=', na))
    } else {
        ## it does have a conditional
        extraE <- 0
        Tsupp[[1]] <- `&&`
        Tcond <- Tsupp[[3]]
        EqA <- c(
            1L * apply(combos, 1, function(zz){
                eval(Tsupp, as.list(zz))
            }),
            0)
        E <- matrix(1, nc + na + 2, na + 1)
        F <- numeric(nc + na + 2)
        E[1, na + 1] <- -1
        E[nc + 1, ] <- c(
            1L * apply(combos, 1, function(zz){
                eval(Tcond, as.list(zz))
            }),
            0)
        F[nc + 1] <- 1
        E[nc + 1 + (1:(na+1)), ] <- diag(na + 1)
        D <- c(rep('==', nc + 1), rep('>=', na + 1))
    }

### Constraint probabilities, analysed one at a time
    Tp <- substitute(alist(...))

    for(i in seq_len(nc)[-1]) {
        if(length(Tp[[i]]) < 3 || !(deparse(Tp[[i]][[1]]) == '==')){
            stop('argument ', i, ' is not an equality')
        }
        left <- Tp[[i]][[2]]
        right <- Tp[[i]][[3]]

        if(!is.numeric(try(eval(right), silent = TRUE))) {
            ## constraint is equality between two probabilities
            if(!(deparse(right[[1]]) %in% c('*', '/'))) {
                temp <- right
                right <- substitute(a * 1)
                right[[2]] <- temp
            }
            coeff <- eval(right[[1]])(1, right[[3]])
            right <- right[[2]]
            if(
                length(left) < 2 || length(left) > 3 ||
                    !(deparse(left[[1]]) %in% Psyms) ||
                    length(right) < 2 || length(right) > 3 ||
                    !(deparse(right[[1]]) %in% Psyms)
                    ) {
                stop('invalid first argument')
            }

            if(
                length(left) == 3 && length(right) == 3
            ) {
                ## both probabilities have conditional
                ## we use the rule of cond. prob.
                if(!(left[[3]] == right[[3]])) {
                    stop('invalid conditionals in argument ', i)
                }
                left[[1]] <- `&&`
                right[[1]] <- `&&`
            } else if (
                !(length(left) == 2) || !(length(right) == 2)
            ) {
                stop('invalid argument ', i)
            }

            E[i, ] <- c(
                1L * apply(combos, 1, function(zz){
                    eval(left, as.list(zz))
                }) - coeff * apply(combos, 1, function(zz){
                    eval(right, as.list(zz))
                }),
                extraE)
        } else {
            ## constraint is numeric
            coeff <- eval(right)

            if(length(left) == 2) {
                left <- left[[2]]
                ## probability doesn't have conditional
                E[i, ] <- c(
                    1L * apply(combos, 1, function(zz){
                        eval(left, as.list(zz))
                    }) - coeff,
                    extraE)
            } else {
                ## probability has conditional
                left[[1]] <- `&&`
                Econd <- left[[3]]
                E[i, ] <- c(
                    1L * apply(combos, 1, function(zz){
                        eval(left, as.list(zz))
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
    ## ## for debugging
    ## print(minp$constraints)
    ## str(minp$solution)
    ## str(minp$objval)
    ## str(minp$status)

    ## Find maximum value
    maxp <- lpSolve::lp(
        direction = 'max',
        objective.in = EqA,
        const.mat = E,
        const.dir = D,
        const.rhs = F
    )
    ## ## for debugging
    ## print(maxp$constraints)
    ## str(maxp$solution)
    ## str(maxp$objval)
    ## str(maxp$status)

    c(
        min = if(minp$status == 0){minp$objval}else{NA},
        max = if(maxp$status == 0){maxp$objval}else{NA}
    )
}
