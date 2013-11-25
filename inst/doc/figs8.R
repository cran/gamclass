
## ----setup, cache=FALSE, echo=FALSE--------------------------------------
library(knitr)
options(replace.assign=FALSE,width=72)
opts_chunk$set(fig.path='figs/deceive-', cache.path='cache/deceive-',
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


## ----fig8_1, eval=TRUE, echo=TRUE----------------------------------------
fig8.1 <- function(plotit=TRUE){
tau <- (0:5)/2.5; m <- length(tau); n <- 200; SD <- 2
x0 <- rnorm(n, mean=12.5, sd=SD)  # Generate x-values
df <- data.frame(sapply(tau, function(xtau)x0+rnorm(n, sd=SD*xtau)))
  # Columns after the first are x-values with added error
df$y = 15+2.5*x0
names(df) <- c(paste("X", tau, sep=""), "y")
lab <- c(list("0"),
         lapply(tau[-1], function(x)substitute(A*s[z], list(A=x))))
form <- formula(paste("y ~ ", paste(paste("X", tau, sep=""),
                                  collapse="+")))
library(latticeExtra)
xlabel <- expression(italic(x)*' ('*italic(z)*' with error)')
striplabel <- strip.custom(strip.names=TRUE,
                           var.name="SD(added err)",
                           sep=expression(" = "),
                           factor.levels=as.expression(lab))
gph <- xyplot(form, data=df, outer=TRUE, xlab=xlabel, strip=striplabel,
               type=c("p", "r"), layout=c(3,2))
gph+layer(panel.abline(15, 2.5, lty=2))
}


## ----fig8_2, eval=TRUE, echo=TRUE----------------------------------------
fig8.2 <- function(){
  gph <- errorsINx(gpdiff=4, , timesSDx=1.25, SDyerr=2.5, n=80, plotit=FALSE)$gph
  gph
}


## ----docheck, eval=TRUE--------------------------------------------------
if(!exists("doFigs")) doFigs <- TRUE


## ----figs8-pkg, eval=doFigs----------------------------------------------
library(DAAG)


## ----figs8_1, eval=doFigs, fig.width=6.5, fig.height=4.75, out.width="0.97\\textwidth"----
gph <- fig8.1()
print(gph)


## ----figs8_2, eval=doFigs, fig.width=6, fig.height=4, out.width="0.97\\textwidth"----
set.seed(31)
gph <- fig8.2()
print(gph)


