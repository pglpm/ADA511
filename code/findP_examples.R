source('findP.R')

## Trivial
findP(p(x))
## min max 
##   0   1 

findP(p(x), p(x) >= 0.3)
## min max 
## 0.3 1.0 

findP(
    p(x),
    p(!x) == 0.5
)
## min max 
## 0.5 0.5

findP(
    p(x ~ I),
    p(!x ~ I) >= 0.3
)
## min max 
## 0.0 0.7 

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
    P(car1 ~ you1 & host2 & I), # target probability
    P(car1 | car2 | car3 ~ I) == 1,
    P(car1 & car2 ~ I) == 0,
    P(car1 & car3 ~ I) == 0,
    P(car2 & car3 ~ I) == 0,
    P(host1 & you1 ~ I) == 0,
    P(host2 & you2 ~ I) == 0,
    P(host3 & you3 ~ I) == 0,
    P(host1 & car1 ~ I) == 0,
    P(host2 & car2 ~ I) == 0,
    P(host3 & car3 ~ I) == 0,
    P(host1 & host2 ~ I) == 0,
    P(host1 & host3 ~ I) == 0,
    P(host2 & host3 ~ I) == 0,
    P(host1 | host2 | host3 ~ I) == 1,
    P(host2 ~ car1 & you1 & I) == P(host3 ~ car1 & you1 & I),
    P(car1 ~ I) == 1/3,
    P(car2 ~ I) == 1/3,
    P(car3 ~ I) == 1/3,
    P(car1 ~ you1 & I) == 1/3,
    P(car2 ~ you1 & I) == 1/3,
    P(car3 ~ you1 & I) == 1/3
 )
##      min      max 
## 0.333333 0.333333 

findP(
    p(c1 ~ y1 & h2 & I),
    p(c1 | c2 | c3 ~ I) == 1,
    p(c1 & c2 ~ I) == 0,
    p(c1 & c3 ~ I) == 0,
    p(c2 & c3 ~ I) == 0,
    p(h1 & y1 ~ I) == 0,
    p(h2 & y2 ~ I) == 0,
    p(h3 & y3 ~ I) == 0,
    p(h1 & c1 ~ I) == 0,
    p(h2 & c2 ~ I) == 0,
    p(h3 & c3 ~ I) == 0,
    p(h1 & h2 ~ I) == 0,
    p(h1 & h3 ~ I) == 0,
    p(h2 & h3 ~ I) == 0,
    p(h1 | h2 | h3 ~ I) == 1,
    p(h2 ~ c1 & y1 & I) == p(h3 ~ c1 & y1 & I),
    p(c1 ~ I) >= 1/3 + 1/6,
    p(c2 ~ I) == 1/3,
    p(c3 ~ I) <= 1/3 - 1/6,
    p(c1 ~ y1 & I) >= 1/3 + 1/6,
    p(c2 ~ y1 & I) == 1/3,
    p(c3 ~ y1 & I) <= 1/3 - 1/6
 )
## min max 
## 0.6 1.0 


