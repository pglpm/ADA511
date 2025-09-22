#' Calculate lower and upper probability bounds
#'
#' @description
#' `inferP()` calculates the minimum and maximum allowed values of the probability for a propositional-logic expression conditional on another one, given numerical or equality constraints for the conditional probabilities for other propositional-logic expressions.
#'
#' @details
#' The function takes as first argument the probability for a logical expression, conditional on another expression, and as subsequent (optional) arguments the constraints on the probabilities for other logical expressions. Propositional logic is intended here.
#'
#' The function uses the [`lpSolve::lp()`] function from the [**lpSolve**](https://cran.r-project.org/package=lpSolve) package.

#'
#' ### Logical expressions
#'
#' A propositional-logic expression is a combination of atomic propositions by means of logical connectives. Atomic propositions can have any name that satisfies [R syntax for *object names*](https://cran.r-project.org/doc/FAQ/R-FAQ.html#What-are-valid-names_003f). Examples:
#' ```
#' a
#' A
#' hypothesis1
#' coin.lands.tails
#' coin_lands_heads
#' `tomorrow it rains` # note the backticks
#' ```
#'
#' Available logical connectives are "not" (negation, "\eqn{\lnot}"), "and" (conjunction, "\eqn{\land}"), "or" (disjunction, "\eqn{\lor}"), "if-then" (implication, "\eqn{\Rightarrow}"). The first three follow the standard R syntax for logical operators (see [base::logical]):
#' - Not: ` !` or ` -`
#' - And: ` & ` or ` && `
#' - Or: ` + `; if argument `solidus = FALSE`, also ` || ` or ` | ` are allowed.
#'
#' The "if-then" connective is represented by the infix operator ` > `; internally `x > y` is simply defined as `x or not-y`.
#'
#' Examples of logical expressions:
#' ```
#' a
#' a & b
#' (a + hypothesis1) & -A
#' red.ball & ((a > !b) + c)
#' ```
#'
#' ### Probabilities of logical expressions
#' 
#' The probability of an expression \eqn{X} conditional on an expression \eqn{Y}in entered with syntax similar to the common mathematical notation \eqn{\mathrm{P}(X \vert Y)}{P(X|Y)}. The solidus "` | `" is used to separate the conditional (note that in usual R syntax such symbol stands for logical "or" instead). If the argument `solidus = FALSE` is given in the function, then the tilde  "` ~ `" is used instead of the solidus (note that in usual R syntax such symbol introduces a formula instead). For instance
#'     
#' \eqn{\mathrm{P}(\lnot a \lor b \:\vert\: c \land H)}{P(not-a or b | c and H)}    
#'     
#' can be entered in the following ways, among others (extra spaces added just for clarity):
#' ```
#' P(!a + b  |  c & H)
#' P(-a + b  |  c && H)
#' P(!a + b  |  c & H)
#' ```
#' or, if argument  `solidus = FALSE`, in the following ways:
#' ```
#' P(!a | b  ~  c & H)
#' P(-a + b  ~  c && H)
#' P(!a || b  ~  c & H)
#' ```
#' It is also possible to use `p` or `Pr` or `pr` instead of `P`.
#'
#' ## Probability constraints
#'
#' Each probability constraint can have one of these four forms:
#' ```
#' P(X | Z) = [number between 0 and 1]
#'
#' P(X | Z) = P(Y | Z)
#'
#' P(X | Z) = P(Y | Z) * [positive number]
#'
#' P(X | Z) = P(Y | Z) / [positive number]
#' ```
#' where `X`, `Y`, `Z` are logical expressions. Note that the conditionals on the left and right sides must be the same. Inequalities `<=` `>=` are also allowed instead of equalities.
#'
#' See the accompanying vignette for more interesting examples.
#'
#' @param target The target probability expression (see Details).
#'
#' @param ... Probability constraints (see Details).
#'
#' @param solidus logical. If `TRUE` (default), the symbol `|` is used to introduce the conditional in the probability; in this case any use of `||` for the 'or'-connective will lead to an error. If `FALSE`, the symbol `~` is used to introduce the conditional; in this case the symbols `|`, `||` can be used for the 'or`-connective.
#'
#' @returns A vector of `min` and `max` values for the target probability, or `NA` if the constraints are mutually contradictory. If `min` and `max` are `0` and `1` then the constraints do not restrict the target probability in any way.
#'
#' @import lpSolve
#'
#' @references
#' T. Hailperin: *Best Possible Inequalities for the Probability of a Logical Function of Events*. Am. Math. Monthly 72(4):343, 1965 <\doi{doi:https://doi.org/10.1080/00029890.1965.11970533}>.
#'
#' T. Hailperin: *Sentential Probability Logic: Origins, Development, Current Status, and Technical Applications*. Associated University Presses, 1996 <https://archive.org/details/hailperin1996-Sentential_probability_logic/>.
#'
#'
#' @examples
#'
#' ## The probability of an "and" is always less
#' ## than the probabilities of the and-ed propositions:
#' inferP(
#'   target = P(a & b | h),
#'   P(a | h) == 0.3,
#'   P(b | h) == 0.6
#' )
#' ## min max
#' ## 0.0 0.3
#'
#' ## P(a & b | h) is completely determined
#' ## by P(a | h) and P(b | a & h):
#' inferP(
#'     target = P(a & b | h),
#'     P(a | h) == 0.3,
#'     P(b | a & h) == 0.2
#' )
#' ##  min  max
#' ## 0.06 0.06
#'
#' ## Solution to the Monty Hall problem (see accompanying vignette):
#' inferP(
#'     target = P(car2  |  you1 & host3 & I),
#'     ##
#'     P(car1 & car2  |  I) == 0,
#'     P(car1 & car3  |  I) == 0,
#'     P(car2 & car3  |  I) == 0,
#'     P(car1 + car2 + car3  |  I) == 1,
#'     P(host1 & host2 | I) == 0,
#'     P(host1 & host3 | I) == 0,
#'     P(host2 & host3 | I) == 0,
#'     P(host1 + host2 + host3  |  I) == 1,
#'     P(host1  |  you1 & I) == 0,
#'     P(host2  |  car2 & I) == 0,
#'     P(host3  |  car3 & I) == 0,
#'     P(car1  |  I) == P(car2  |  I),
#'     P(car2  |  I) == P(car3  |  I),
#'     P(car1  |  you1 & I) == P(car2  |  you1 & I),
#'     P(car2  |  you1 & I) == P(car3  |  you1 & I),
#'     P(host2  |  you1 & car1 & I) == P(host3  |  you1 & car1 & I)
#' )
#' ##      min      max
#' ## 0.666667 0.666667
#'
#' @export
inferP <- function(target, ..., solidus = TRUE) {
    ## Logical connectives
    if(solidus){
        connectives <- list(
            `&` = .Primitive("&&"),
            `+` = .Primitive("||"),
            `-` = .Primitive("!"),
            `>` = function(x, y){y || !x}
        )
        bar <- '|'
        ## Check if "||" is used
        if(!all( gregexpr(' || ',
            deparse(as.formula(substitute(~ alist(target, ...)))),
            fixed = TRUE)[[1]] == -1 )){
            stop("When argument 'solidus = TRUE', use of '||' is not allowed")
            }

    } else {
        connectives <- list(
            `&` = .Primitive("&&"),
            `+` = .Primitive("||"),
            `-` = .Primitive("!"),
            `>` = function(x, y){y || !x}
        )
        bar <- '~'
    }
    barmatch <- paste0(' ', bar, ' ')

    ## number of constraints
    nc <- length(substitute(alist(...)))

    ## find atomic sentences and prepare truth table for DNF
    tvals <- c(FALSE, TRUE)
    atoms <- sort(all.vars(stats::as.formula(substitute(~ alist(target, ...)))))
    ttable <- list()
    for(i in atoms) {
        ttable[[i]] <- tvals
    }
    combos <- expand.grid(ttable)

    ## total number of conjunctions
    na <- 2L^length(atoms)

    ## print(combos) # for debugging

    ## Accepted probability (or truth) symbols
    Psyms <- c('P', 'p', 'Pr', 'pr', 'T', 't')
    ## accepted relation symbols
    Esyms <- c('==', '<=', '>=', '<', '>')

    ## Below:
    ## E: matrix of constraint coefficients
    ## F: vector of numerical values of constraints
    ## D: vector of equality/inequality constraint directions

### Target probability
    Tsupp <- substitute(target)

    ## Syntax check
    if(
        !(length(Tsupp) == 2) ||
            !(deparse(Tsupp[[1]]) %in% Psyms) ||
             length(gregexpr(barmatch, deparse(Tsupp), fixed = TRUE)[[1]]) > 1
    ) {
        stop('invalid target')
    }
    Tsupp <- Tsupp[[2]]

    if(length(Tsupp) < 3 || !(deparse(Tsupp[[1]]) == bar)){
        ## it doesn't have a conditional
        extraE <- NULL
        Obj <- 1L * apply(combos, 1, function(zz){
            eval(Tsupp, c(connectives, as.list(zz)))
        })
        E <- matrix(1, nc + na, na)
        F <- numeric(nc + na)
        F[1] <- 1
        E[nc + 1:na, ] <- diag(na)
        D <- c(rep('==', nc), rep('>=', na))

    } else {
        ## it does have a conditional
        extraE <- 0
        Tsupp[[1]] <- `&&`
        Tcond <- Tsupp[[3]]
        Obj <- c(
            1L * apply(combos, 1, function(zz){
                eval(Tsupp, c(connectives, as.list(zz)))
            }),
            0)
        E <- matrix(1, nc + na + 2, na + 1)
        F <- numeric(nc + na + 2)
        ## first constraint has the form sum_i x_i - t = 0
        E[1, na + 1] <- -1
        ## last constraint involves the conditional of target prob.
        E[nc + 1, ] <- c(
            1L * apply(combos, 1, function(zz){
                eval(Tcond, c(connectives, as.list(zz)))
            }),
            0)
        F[nc + 1] <- 1
        E[nc + 1 + 1:(na+1), ] <- diag(na + 1)
        D <- c(rep('==', nc + 1), rep('>=', na + 1))
    }

### Constraint probabilities, analysed one at a time
    Tp <- substitute(alist(...))

    for(i in seq_len(nc)[-1]) {

        ## Syntax check
        if(length(Tp[[i]]) < 3 || !(deparse(Tp[[i]][[1]]) %in% Esyms)){
            stop('argument ', i, ' is not an (in)equality')
        }

        D[i] <- deparse(Tp[[i]][[1]]) # sign of (in)equality
        left <- Tp[[i]][[2]]

        ## Syntax check
        if(
            !(length(left) == 2) ||
                !(deparse(left[[1]]) %in% Psyms) ||
                 length(gregexpr(barmatch, deparse(left), fixed = TRUE)[[1]]) > 1
        ) {
            stop('invalid left side in argument ', i)
        }

        right <- Tp[[i]][[3]]

        if(!is.numeric(try(eval(right), silent = TRUE))) {
            ## constraint is equality between two probabilities
            if(
                length(right) < 2 || length(right) > 3
            ) {
                stop('invalid right side in argument ', i)
            }

            if(length(right) == 2) {
                ## right side is a probability
                if(
                    !(deparse(right[[1]]) %in% Psyms) ||
                        length(gregexpr(barmatch, deparse(right), fixed = TRUE)[[1]]) > 1
                ) {
                    stop('invalid right side in argument ', i)
                }
                temp <- right
                right <- substitute(a * 1)
                right[[2]] <- temp
            }

            if(
                !(deparse(right[[1]]) %in% c('*', '/')) ||
                    !(length(right[[2]]) == 2) ||
                     !(deparse(right[[2]][[1]]) %in% Psyms)
            ) {
                stop('invalid right side in argument ', i)
            }
            coeff <- eval(right[[1]])(1, right[[3]])
            right <- right[[2]]

            Esuppl <- left[[2]]
            Esuppr <- right[[2]]
            if(
                !(length(Esuppl) < 3 || !(deparse(Esuppl[[1]]) == bar)) ||
                !(length(Esuppr) < 3 || !(deparse(Esuppr[[1]]) == bar))
            ) {
                ## both probabilities have conditional
                ## we use the rule of cond. prob.
                if(!(Esuppl[[3]] == Esuppr[[3]])) {
                    stop('invalid conditionals in argument ', i)
                }
                Esuppl[[1]] <- `&&`
                Esuppr[[1]] <- `&&`
            }

            E[i, ] <- c(
                1L * apply(combos, 1, function(zz){
                    eval(Esuppl, c(connectives, as.list(zz)))
                }) - coeff * apply(combos, 1, function(zz){
                    eval(Esuppr, c(connectives, as.list(zz)))
                }),
                extraE)
        } else {
            ## constraint is numeric
            coeff <- eval(right)
            if(coeff < 0) {
                stop('negative coefficient in argument ', i)
            }

            Esupp <- left[[2]]
            if(length(Esupp) < 3 || !(deparse(Esupp[[1]]) == bar)) {
                ## probability has no conditional
                E[i, ] <- c(
                    1L * apply(combos, 1, function(zz){
                        eval(Esupp, c(connectives, as.list(zz)))
                    }) - coeff,
                    extraE)
            } else {
                ## probability has conditional
                Esupp[[1]] <- `&&`
                Econd <- Esupp[[3]]
                E[i, ] <- c(
                    1L * apply(combos, 1, function(zz){
                        eval(Esupp, c(connectives, as.list(zz)))
                    }) -
                        coeff * apply(combos, 1, function(zz){
                            eval(Econd, c(connectives, as.list(zz)))
                        }),
                    extraE)
            }
        }
    }

    ## print(list(E = E, F = F, EqA = Obj)) # for debugging
    ## Find minimum value
    minp <- lpSolve::lp(
        direction = 'min',
        objective.in = Obj,
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
        objective.in = Obj,
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
        min = if(minp$status == 0){minp$objval} else {NA},
        max = if(maxp$status == 0){maxp$objval} else {NA}
    )
}
