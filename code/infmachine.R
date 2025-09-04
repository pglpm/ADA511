source('findP.R')

findP(
    p(x),
    p(!x) == 0.5
)
## min max 
## 0.5 0.5

findP(
    p(x ~ I),
    p(!x ~ I) == 0.5
)
## min max 
## 0.5 0.5

findP(
    p(x || y ~ I),
    p(x ~ I) == 0.3,
    p(y ~ I) == 0.5,
    p(x & y ~ I) == 0.1
)
## min max 
## 0.7 0.7 

findP(
    p(x ~ a & b),
    p(x ~ a) == 0.5,
    p(x ~ b) == 0.3
)
## min max 
##   0   1 

findP(
    p(a & b ~ z),
    p(a ~ b & z) == 1/2,
    p(b ~ z) == 0.3
)
##  min  max 
## 0.15 0.15 

findP(
    p(a ~ b & z),
    p(a & b ~ z) == 0.15,
    p(b ~ z) == 0.3
)
##  min max 
##  0.5 0.5 

findP(
    P(x ~ !x & I),
    P(x ~ I) == 1
)
## min max 
##  NA  NA 

findP(
    P(x ~ y & I),
    P(x ~ I) == 1
)
## min max 
##   1   1 


findP(
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
##      min      max 
## 0.333333 0.333333 

findP(
    p(c1 ~ y1 & h2),
    ## p(c1 | c2 | c3) == 1,
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
##      min      max 
## 0.333333 0.333333 

findP(
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
    p(h2 ~ c1 & y1) == p(h3 ~ c1 & y1),
    p(c1) == 1/3,
    p(c2) == 1/3,
    p(c3) == 1/3,
    p(c1 ~ y1) == 1/3,
    p(c2 ~ y1) == 1/3,
    p(c3 ~ y1) == 1/3
 )
##      min      max 
## 0.333333 0.333333 

