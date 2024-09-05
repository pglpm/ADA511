mystartup()

#####################################################
#### Same initial belief, different resistances
#####################################################
set.seed(20)
values <- c('SSD', 'Motherboard', 'CPU', 'Keyboard', 'Screen', 'GPU', 'PCI')
nv <- length(values)
##
pstrength1 <- nv*2
pstrength2 <- 0.01
alpha1 <- rep(pstrength1/nv,nv)
alpha2 <- rep(pstrength2/nv,nv)
names(alpha1) <- names(alpha2) <- values
##
probsv <- c(8, 8, 1, 35, 8, 15, 25)/100
## probsv <- c(0.1, 16, 1.7, 0.1, 0.1, 32, 50)/100
##
ndatum <- 1
nupd <- 200
pdff('example_update_laptop_quickbeliever')
for(i in 1:(nupd+1)){
    tplot(x=values, y=list(tnormalize(alpha1), tnormalize(alpha2)), type='b',
        col=1:2, xlab=NA, ylab='belief about next laptop', xlim=c(0.85, nv+0.15),
        ylim=c(0,1))
    tlegend(legend=paste0('AI technician ',1:2),'topleft', cex=1.25)
    tlegend(legend=paste0('# observations: ',i-1),'topright', cex=1.25, lty=NA, pch=NA)
    tlegend(legend=(if(i > 1){
        paste0('laptop ',i-1,'\n',datum)
    }else{
        'initial\nbelief'
    }),
    x=ndatum,
    y=0.8,
    ## y=max(tnormalize(alpha1)[ndatum],tnormalize(alpha2)[ndatum])+0.2,
    xjust=0.7, cex=1.25,lty=NA,pch=NA)
    ##
    ndatum <- sample(1:length(values), 1, prob=probsv)
    datum <- values[ndatum]
    ##
    alpha1[datum] <- alpha1[datum] + 1
    alpha2[datum] <- alpha2[datum] + 1
    ## invisible(scan("stdin", character(), nlines = 1, quiet = TRUE))
    ## Sys.sleep(round(3*exp(-i/10)))
}
dev.off()



#####################################################
#### Different initial beliefs, same resistances
#####################################################
set.seed(20)
values <- c('SSD', 'Motherboard', 'CPU', 'Keyboard', 'Screen', 'GPU', 'PCI')
nv <- length(values)
##
pstrength1 <- nv
pstrength2 <- nv
alpha1 <- rep(pstrength1/nv,nv)
alpha2 <- pstrength2*tnormalize((nv:1)^5)
names(alpha1) <- names(alpha2) <- values
##
probsv <- c(8, 8, 1, 35, 8, 15, 25)/100
## probsv <- c(0.1, 16, 1.7, 0.1, 0.1, 32, 50)/100
##
ndatum <- 1
nupd <- 200
pdff('example_update_laptop_differentstart')
for(i in 1:(nupd+1)){
    tplot(x=values, y=list(tnormalize(alpha1), tnormalize(alpha2)), type='b',
        col=1:2, xlab=NA, ylab='belief about next laptop', xlim=c(0.85, nv+0.15),
        ylim=c(0,1))
    tlegend(legend=paste0('AI technician ',1:2),'topleft', cex=1.25)
    tlegend(legend=paste0('# observations: ',i-1),'topright', cex=1.25, lty=NA, pch=NA)
    tlegend(legend=(if(i > 1){
        paste0('laptop ',i-1,'\n',datum)
    }else{
        'initial\nbelief'
    }),
    x=ndatum,
    y=0.8,
    ## y=max(tnormalize(alpha1)[ndatum],tnormalize(alpha2)[ndatum])+0.2,
    xjust=0.7, cex=1.25,lty=NA,pch=NA)
    ##
    ndatum <- sample(1:length(values), 1, prob=probsv)
    datum <- values[ndatum]
    ##
    alpha1[datum] <- alpha1[datum] + 1
    alpha2[datum] <- alpha2[datum] + 1
    ## invisible(scan("stdin", character(), nlines = 1, quiet = TRUE))
    ## Sys.sleep(round(3*exp(-i/10)))
}
dev.off()




## values <- c('SSD', 'Motherboard', 'CPU', 'Keyboard', 'Screen', 'GPU', 'PCI')
## nv <- length(values)
## ##
## pstrength1 <- nv
## pstrength2 <- nv
## alpha1 <- rep(pstrength1/nv,nv)
## alpha2 <- pstrength2*tnormalize((nv:1)^5)
## names(alpha1) <- names(alpha2) <- values
## ##
## probsv <- c(8, 8, 1, 35, 8, 15, 25)/100
## ## probsv <- c(0.1, 16, 1.7, 0.1, 0.1, 32, 50)/100
## ##
## ndatum <- 1
## i <- 1
## tplot(x=values, y=list(tnormalize(alpha1), tnormalize(alpha2)), type='b',
##       col=1:2, xlab=NA, ylab='belief about next laptop', xlim=c(0.85, nv+0.15),
##       ylim=c(0,1))
## tlegend(legend=paste0('technician ',1:2),'topleft', cex=1.5)
## tlegend(legend=(if(i > 1){
##                     paste0('laptop ',i,'\n',datum)
##                 }else{
##                     'initial\nbelief'
##                 }),
##         x=ndatum,
##         y=0.8,
##         ## y=max(tnormalize(alpha1)[ndatum],tnormalize(alpha2)[ndatum])+0.2,
##         xjust=0.7, cex=1.25,lty=NA,pch=NA)
## 
## nupd <- 200
## for(i in 2:(nupd+1)){
##     tplot(x=values, y=list(tnormalize(alpha1), tnormalize(alpha2)), type='b',
##           col=1:2, xlab=NA, ylab='belief', xlim=c(0.85, nv+0.15),
##           ylim=c(0,1))
##     tlegend(legend=paste0('technician ',1:2),'topleft', cex=1.5)
##     tlegend(legend=(if(i > 1){
##                         paste0('laptop ',i-1,'\n',datum)
##                     }else{
##                         'initial\nbelief'
##                         }),
##             x=ndatum,
##             y=0.8,
##             ## y=max(tnormalize(alpha1)[ndatum],tnormalize(alpha2)[ndatum])+0.2,
##             xjust=0.7, cex=1.25,lty=NA,pch=NA)
##     ##
##     ndatum <- sample(1:length(values), 1, prob=probsv)
##     datum <- values[ndatum]
##     ##
##     alpha1[datum] <- alpha1[datum] + 1
##     alpha2[datum] <- alpha2[datum] + 1
##     invisible(scan("stdin", character(), nlines = 1, quiet = TRUE))
##     ## Sys.sleep(round(3*exp(-i/10)))
## }
