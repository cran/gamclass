
## ----setup, cache=FALSE, echo=FALSE--------------------------------------
library(knitr)
options(replace.assign=FALSE,width=72)
opts_chunk$set(fig.path='figs/ord-', cache.path='cache/ord-',
               fig.align='center', dev='pdf', fig.width=3.5,
               fig.height=3.5, out.width="0.8\\textwidth", 
               fig.show='hold', par=TRUE,
               tidy=FALSE,  comment=NA)
knit_hooks$set(par=function(before, options, envir){
if (before && options$fig.show!='none') par(mar=c(4,4,1.6,.1),
              cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3)
}, crop=hook_pdfcrop)
pdf.options(pointsize=12)
oldopt <- options(digits=4)


## ----fig9_1, eval=TRUE, echo=TRUE----------------------------------------
fig9.1 <- function(){
  opar <- par(xpd=TRUE)
  if(!exists("aupoints")){
    cat("Trying to obtain audists from DAAG")        
    if(!require(DAAG))stop("'DAAG' must be installed")
    aupoints <- cmdscale(audists)
  }
    assign('aupoints', aupoints, pos=1)
    xlim <- range(aupoints[,1])
    xlim <- xlim + diff(xlim)*c(-0.05, 0.2)
    plot(aupoints, bty="n", xlab="", ylab="", xlim=xlim)
    labs <- rownames(aupoints)
    labpos <- rep(1, length(labs))
    labpos[labs%in%c("Cairns","Melbourne")] <- 3
    labpos[labs=="Canberra"] <- 4
    text(aupoints, labels=labs, pos=labpos, xpd=TRUE)
  par(opar)
}


## ----cfPhysical,  eval=TRUE, echo=TRUE-----------------------------------
comparePhysical <- function(lat=aulatlong$latitude,
                            long=aulatlong$longitude,
                            x1=aupoints[,1], x2 = aupoints[,2],
                            wts=NULL){
    ## Get best fit in space of (latitude, longitude)
    if(is.null(wts))wts <- rep(1,length(x1))
    fitlat <- predict(lm(lat ~ x1+x2, weights=wts))
    fitlong <- predict(lm(long ~ x1+x2, weights=wts))
    x <- as.vector(rbind(lat, fitlat, rep(NA,10)))
    y <- as.vector(rbind(long, fitlong, rep(NA,10)))
    lines(x, y, col="gray40", lwd=3)
}


## ----fig9_2A, eval=TRUE, echo=TRUE---------------------------------------
fig9.2A <- function(){
    if(!require(DAAG))stop("'DAAG' must be installed")
    if(!require(oz))stop("Package 'oz' must be installed")
    if(!exists('aupoints'))aupoints <- cmdscale(audists)
    oz()
    points(aulatlong, col="red", pch=16, cex=1.5)
    comparePhysical(x1=aupoints[,1], x2 = aupoints[,2])
}


## ----fig9_2B, eval=TRUE, echo=TRUE---------------------------------------
fig9.2B <- function(){
    if(!require(MASS))stop("Package 'MASS' must be installed")
    if(!require(oz))stop("Package 'oz' must be installed")
    aupoints.sam <- sammon(audists, trace=FALSE)
    oz()
    points(aulatlong, col="red", pch=16, cex=1.5)
    wt <- apply(as.matrix(audists), 1,function(x)sum(1/x[x>0]))
    comparePhysical(x1=aupoints.sam$points[,1],
                    x2 = aupoints.sam$points[,2], wts=wt)
}


## ----fig9_2, eval=TRUE, echo=TRUE----------------------------------------
fig9.2 <- function(){
  par(fig=c(0,1,0.5,1))
  fig9.2A()
  par(fig=c(0,1,0,0.5), new=TRUE)  
  fig9.2B()  
}


## ----fig9_3A, eval=TRUE, echo=TRUE---------------------------------------
fig9.3A <- function(seed=47, xlab="Axis 1", ylab="Axis 2"){
    if(!require(DAAGbio))stop("Package 'DAAGbio' must be installed")
    if(!require(ape))stop("Package 'ape' must be installed")
    ## Calculate distances, using Kimura's K80 model
    primates.dist <- dist.dna(as.DNAbin(primateDNA), model="K80")
    primates.cmd <- cmdscale(primates.dist)
    eqscplot(primates.cmd, xlab=xlab, ylab=ylab, cex.lab=1.15)
    lefrt <- 2+2*(primates.cmd[,1] < mean(par()$usr[1:2]))
    text(primates.cmd[,1], primates.cmd[,2], row.names(primates.cmd),
         pos=lefrt)
}


## ----fig9_3B, eval=TRUE, echo=TRUE---------------------------------------
fig9.3B <- function(seed=47, xlab="Axis 1", ylab="Axis 2"){
    if(!require(DAAGbio))stop("Package 'DAAGbio' must be installed")
    if(!require(ape))stop("Package 'ape' must be installed")
    if(!require(MASS))stop("Package 'MASS' must be installed")
    primates.dist <- dist.dna(as.DNAbin(primateDNA), model="K80")
    primates.cmd <- cmdscale(primates.dist)
    primates.mds <- isoMDS(primates.dist, primates.cmd, k=2, trace=FALSE)
    eqscplot(primates.mds$points, xlab=xlab, ylab=ylab,
             cex.lab=1.15)
    lefrt <- 2+2*(primates.mds$points[,1] < mean(par()$usr[1:2]))
    text(primates.mds$points[,1], primates.mds$points[,2],
         row.names(primates.mds$points), pos=lefrt)
}


## ----fig9_3, eval=TRUE, echo=TRUE----------------------------------------
fig9.3 <- function(){
  opar <- par(fig=c(0,0.5,0,1), mar=c(3.1,3.1,1.6,0.1))
  fig9.3A()
  par(fig=c(0.5,1,0,1), new=TRUE)  
  fig9.3B(ylab="")
  par(fig=c(0,1,0,1))
  par(opar)
}


## ----fig9_4, eval=TRUE, echo=TRUE----------------------------------------
fig9.4 <- function(){
    if(!require(DAAG))stop("Package 'DAAG' must be installed")
    if(!require(MASS))stop("Package 'MASS' must be installed")
    pacific.dist <- dist(x = as.matrix(rockArt[-c(47,54,60,63,92),
                         28:641]), method = "binary")
    sum(pacific.dist==1)/length(pacific.dist)
    ## Now check that in all columns at least one distance < 1
    symmat <- as.matrix(pacific.dist)
    checksum <- sum(apply(symmat, 2, function(x) sum(x<1)))
    checksum <- sum(apply(symmat, 2, function(x) sum(x<1)==0))
    print(c("No of cols where all distances are one"=checksum))
    pacific.cmd <- cmdscale(pacific.dist)
    pacific.mds <- isoMDS(pacific.dist, pacific.cmd, trace=FALSE)
    plot(pacific.mds$points)
}


## ----docheck, eval=TRUE--------------------------------------------------
if(!exists("doFigs")) doFigs <- TRUE


## ----figs9-load-pkgs, eval=doFigs----------------------------------------
pkgs <- "DAAGbio"
z <- sapply(pkgs, require, character.only=TRUE, warn.conflicts=FALSE)
if(any(!z)){
  notAvail <- paste(names(z)[!z], collapse=", ")
  stop(paste("The following packages should be installed:", notAvail))
}


## ----fig9_1x, eval=doFigs, echo=TRUE, fig.width=5, fig.height=5----------
fig9.1()


## ----aupoints, eval=doFigs-----------------------------------------------
if(!exists("aupoints")) 
aupoints <- cmdscale(audists)


## ----fig9_2x, eval=doFigs, echo=TRUE, fig.width=5, fig.height=8----------
fig9.2()


## ----fig9_3x, eval=doFigs, echo=TRUE, fig.width=7.5, fig.height=4, out.width="0.97\\textwidth"----
fig9.3()


## ----fig9_4x, eval=doFigs, echo=TRUE, fig.width=4, fig.height=4.25-------
fig9.4()


## ----restore, eval=TRUE, echo=FALSE--------------------------------------
options(continue="+ ", prompt="> ")


