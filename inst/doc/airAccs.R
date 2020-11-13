## ----setup, cache=FALSE, echo=FALSE-----------------------------------
library(knitr)
options(replace.assign=FALSE,width=72)
opts_chunk$set(fig.align='center', fig.width=5.5,
               fig.height=4.25, par=TRUE,
               tidy=FALSE,  comment=NA)
knit_hooks$set(par=function(before, options, envir){
if (before && options$fig.show!='none') par(mar=c(4,4,2.6,.1),
              cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3)
}, crop=hook_pdfcrop)
oldopt <- options(digits=4)

## ----eventCounts------------------------------------------------------
airAccs <- gamclass::airAccs
fromDate <- as.Date("2006-01-01")
dfDay06 <- gamclass::eventCounts(airAccs, dateCol="Date",
                       from= fromDate, by="1 day",
                       prefix="num")
dfDay06$day <- julian(dfDay06$Date, origin=fromDate)
dfWeek06 <- gamclass::eventCounts(airAccs, dateCol="Date", from=fromDate,
                        by="1 week", prefix="num")
dfWeek06$day <- julian(dfWeek06$Date, origin=fromDate)

## ----airAccsCap-------------------------------------------------------
cap1 <- "Figure 1: Fitted number of events (aircraft accidents) per week
  versus time."

## ----planeCrash, out.width='60%', fig.width=6, fig.height=4, fig.cap=cap1----
library(grid)
library(mgcv)
year <- seq(from=fromDate, to=max(dfDay06$Date), by="1 year")
atyear=julian(year, origin=fromDate)
dfWeek06.gam <- gam(num~s(day, k=200), data=dfWeek06, family=quasipoisson)
av <- mean(predict(dfWeek06.gam))
plot(dfWeek06.gam, xaxt="n", shift=av, trans=exp, rug=FALSE,
     xlab="", ylab="Estimated rate per week", fg='gray')
axis(1, at=atyear, labels=format(year, "%Y"))
grid(lw=2,nx=NA,ny=NULL)
abline(v=atyear, lwd=2, lty=3, col='lightgray')
mtext(side=3, line=0.75, "Figure 1: Events per week, vs date", cex=1.25, adj=0)

## ----cap2-------------------------------------------------------------
cap2 <- "Figure 2: Fitted number of events (aircraft accidents) per day
  versus time."

## ----eventsPerDay, fig.width=6, fig.height=4, fig.cap=cap2, eval=FALSE----
#  dfDay06.gam <- , out.width='60%', fig.width=6, fig.height=4, fig.cap=cap2}
#    gam(formula = num ~ s(day, k=200), family = quasipoisson,
#        data = dfDay06)
#  av <- mean(predict(dfDay06.gam))
#  plot(dfDay06.gam, xaxt="n", shift=av, trans=exp, rug=FALSE,
#       xlab="", ylab="Estimated rate per day", fg='gray')
#  axis(1, at=atyear, labels=format(year, "%Y"))
#  grid(lw=2,nx=NA,ny=NULL)
#  abline(v=atyear, lwd=2, lty=3, col='lightgray')
#  mtext(side=3, line=0.75, "A: Events per day, vs date", cex=1.25, adj=0)}

