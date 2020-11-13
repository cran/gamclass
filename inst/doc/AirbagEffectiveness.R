## ----setup, cache=FALSE, echo=FALSE-----------------------------------
library(knitr)
options(replace.assign=FALSE,width=72)
opts_chunk$set(fig.align='center', fig.width=6.25,
               fig.height=4.25, tidy=FALSE, comment=NA)
knit_hooks$set(par=function(before, options, envir){
if (before && options$fig.show!='none') par(mar=c(4,4,2.6,.1),
              cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3)
}, crop=hook_pdfcrop)
oldopt <- options(digits=4)

## ----ratio1998--------------------------------------------------------
library(gamclass)
tabAaDeaths <- tabFarsDead(dset=FARS)[['airbagAvail']]["1998", , ]
round(tabAaDeaths, 3)

## ----cap1-------------------------------------------------------------
cap1 <- 'With/without ratio of the ratio of driver death rates
  to passenger death rates:
  (a) w/wo driver airbag (not available for 2009 and 2010); (b) w/wo airbag deployment,
  (c) w/wo driver restraint (mainly seatbelt).'

## ----plotFars, fig.cap=cap1, out.width='60%', fig.show='hold'---------
plotFars(tabDeaths=gamclass::frontDeaths,
          statistics = c("airbagAvail", "airbagDeploy", "restraint"))

