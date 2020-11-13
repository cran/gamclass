## ----setup, cache=FALSE, echo=FALSE-----------------------------------
library(knitr)
options(replace.assign=FALSE,width=72)
opts_chunk$set(fig.path='figs/key-', cache.path='cache/key-',
               fig.align='center', fig.width=3.5,
               fig.height=3.5, fig.show='hold', par=TRUE,
               tidy=FALSE,  comment=NA)
knit_hooks$set(par=function(before, options, envir){
if (before && options$fig.show!='none') par(mar=c(4,4,2.6,.1),
              cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3)
}, crop=hook_pdfcrop)
oldopt <- options(digits=4)

## ----cap123-----------------------------------------------------------
cap1 <- "Heart weight versus body weight of 97 male cats.
A regression line has been fitted."
cap2 <- "Q-Q plot ('Normal Probability plot') of residuals from
the regression of heart weight on body weight, for the male cats."
cap3 <- "Scatterplot matrix of bootstrap estimates of `Slope`
versus `Intercept`, with boxplots used to summarize the marginal
distributions."
cap4 <- "Bootstrap resampling result, compared with simulation
  resampling."

## ----cats-xy, fig.width=4.25, fig.height=4.25, fig.cap=cap1-----------
library(lattice)
mcats <- subset(MASS::cats, Sex=="M")
xyplot(Hwt ~ Bwt, data=mcats, type=c("p","r"))

## ----mcats-lm---------------------------------------------------------
mcats.lm <- lm(Hwt~Bwt, data=mcats)
signif(coef(summary(mcats.lm)),3)

## ----cats-qq, fig.cap=cap2--------------------------------------------
res <- resid(mcats.lm)
qqnorm(res)

## ----outlier-num}, eval=TRUE, echo=TRUE-------------------------------
nout <- (1:nrow(mcats))[c(which.min(res),which.max(res))]

## ----boot-regcoefs, fig.cap=cap3, fig.width=2.85, fig.height=3--------
library(gamclass, quietly=TRUE)
library(car, quietly=TRUE)
mcats <- subset(MASS::cats, Sex=="M")
bootmat <- bootreg(Hwt ~ Bwt,
                   data = mcats,
                   nboot = 1000)
bootdf <- as.data.frame(bootmat)
names(bootdf) <- c("Intercept",
                   "Slope")
colr <- adjustcolor(rep("black",3),
                    alpha.f=0.25)
scatterplot(Slope ~ Intercept,
            col=colr, data=bootdf,
            boxplots="xy",
            regLine=NA,
            smooth=FALSE)

## ----95CI-------------------------------------------------------------
sapply(bootdf, quantile, prob=c(.025, .975))

## ----alt-boot-omitOutlier---------------------------------------------
library(lattice)
bootmat <- bootreg(formula = Hwt ~ Bwt,
                   data = mcats[-97, ],
                   nboot = 1000)
bootdf0 <- as.data.frame(bootmat)
names(bootdf0) <- c("Intercept","Slope")
gphA <- xyplot(Slope ~ Intercept, data=bootdf0, alpha=0.25)

## ----alt-sim----------------------------------------------------------
simmat <- simreg(formula = Hwt ~ Bwt,
                 data=mcats[-97, ], nsim=1000)
simdf <- as.data.frame(simmat)
names(simdf) <- c("Intercept","Slope")
gphB <- xyplot(Slope ~ Intercept, data=simdf, alpha=0.25)

## ----alt-resamp, out.width='47%', fig.show='hold', fig.width=3.85, fig.height=3.85, fig.cap=cap4----
update(gphA, main=grid::textGrob("A: Bootstrap (outlier omitted)",
       x=grid::unit(.05, "npc"),
       y = grid::unit(.25, "npc"), just="left",
       gp=grid::gpar(cex=1)))
update(gphB, main=grid::textGrob("B: Simulation",
       x = grid::unit(.05, "npc"),
       y = grid::unit(.25, "npc"), just="left",
       gp=grid::gpar(cex=1)))

## ----cap5-------------------------------------------------------------
cap5 <- "Regression tree for predicting `Mileage` given
`Weight`. At each node, observations for which the
criterion is satisfied take the branch to the left.  Thus,
at the first node, `tonsWt$>=$1.218` chooses the branch to
the left, while `tonsWt$<$1.218` chooses the branch to the
right.  Panel B plots `Mileage` versus `tonsWt`, with
fitted values from the `rpart` model shown as horizontal 
grey lines."

## ----rpart------------------------------------------------------------
library(rpart)
Car90 <- na.omit(car90[, c("Mileage","Weight")])
## Express weight in metric tonnes
Car90 <- within(Car90, tonsWt <- Weight/2240)
car90.rpart <- rpart(Mileage ~ tonsWt, data=Car90)

## ----deftree, out.width='48%', fig.cap=cap5,fig.width=5.25, fig.height=5.25, fig.show='hold'----
## Code for Panel A
plot(car90.rpart)
text(car90.rpart, xpd=TRUE, digits=3)
mtext(side=3, line=1.25, "A: Regression tree", adj=0, cex=1.25)
## Code for Panel B
plot(Mileage ~ tonsWt, data=Car90, fg='gray')
wt <- with(Car90, tonsWt)
hat <- predict(car90.rpart)
gamclass::addhlines(wt, hat, lwd=2, col="gray")
mtext(side=3, line=1.25, "B: Predicted values from tree", adj=0, cex=1.25)

## ----cap6-------------------------------------------------------------
cap6 <- "Between group sum of squares for `Mileage`, as a
function of the value of `tonsWt` at which the split is
made. The choice $c = 1.218$ maximizes the between groups
sum of squares."

## ----bss-plot, fig.width=5, h=4.25, out.width="60%", fig.cap=cap6-----
## Code
BSS <- bssBYcut(tonsWt, Mileage, Car90)
with(BSS, plot(xOrd, bss, xlab="Cutpoint", fg='gray', ylab=""))
mtext(side=2, line=1.5, "Between groups\nsum of squares")
abline(v=1.218, lty=2)

## ----cap7, out.width='80%'--------------------------------------------
cap7 <- "Scatterplot matrix of accuracies for the several models.
         The line $y=x$ is shown in each panel.
         Note that `rfOOB` is out-of-bag accuracy, i.e., calculated from
         the set of 95 observations, and that `rfTest` is accuracy on the
         test data, again for a random forest model with no preliminary
         smoothing. Results from hybrid models are labeled according to
         the name of the formula for the smooth.   The final accuracy,
         evaluated on the test data, is for a random forest model
         fitted to residuals from the smooth"

## ----meuse-setup------------------------------------------------------
data("meuse", package="sp")
meuse <- within(meuse, {levels(soil) <- c("1","2","2")
                        ffreq <- as.numeric(ffreq)
                        loglead <- log(lead)
})

## ----gam-formulae-----------------------------------------------------
form1 <- ~ dist + elev + soil + ffreq
form3 <- ~ s(dist, k=3) + s(elev,k=3) + soil +ffreq
form3x <- ~ s(dist, k=3) + s(elev,k=3) + s(x, k=3) + soil+ffreq
form8x <- ~ s(dist, k=8) + s(elev,k=8) + s(x, k=8) + soil+ffreq
formlist <- list("Hybrid1"=form1, "Hybrid3"=form3, "Hybrid3x"=form3x,
                 "Hybrid8x"=form8x)

## ----rfgam-setup------------------------------------------------------
rfVars <- c("dist", "elev", "soil", "ffreq", "x", "y")
nrep <- 100
errsmat <- matrix(0, nrep, length(formlist)+2)
dimnames(errsmat)[[2]] <- c(names(formlist), "rfTest", "rfOOB")

## ----rfgam-many-------------------------------------------------------
n <- 95
for(i in 1:nrep){
sub <- sample(1:nrow(meuse), n)
meuseOut <- meuse[-sub,]
meuseIn <- meuse[sub,]
errsmat[i, ] <- gamclass::gamRF(formlist=formlist, yvar="loglead",
                                rfVars=rfVars, data=meuseIn,
                      newdata=meuseOut, printit=FALSE)
}

## ----cf-models-rfgam, fig.cap=cap7, fig.width=7.5, fig.height=7.5, out.width='65%'----
## Code for scatterplot of results
ran <- range(errsmat)
at <- round(ran+c(0.02,-0.02)*diff(ran),2)
lis <- list(limits=ran, at=at, labels=format(at, digits=2))
lims=list(lis,lis,lis,lis,lis,lis)
library(lattice)
splom(errsmat,
      pscales=lims,
      par.settings=simpleTheme(cex=0.75),
      col=adjustcolor("black", alpha=0.5),
      panel=function(x,y,...){lpoints(x,y,...)
      panel.abline(0,1,col="gray")}
)

## ----cf-accs-rfgam----------------------------------------------------
errdf <- stack(as.data.frame(errsmat[,-6]))
names(errdf) <- c("errs","model")
errdf$model <- relevel(errdf$model, ref="rfTest")
errdf$sample <- factor(rep(1:nrow(errsmat),5))
errors.aov <- aov(errs ~ model+sample, data=errdf)
round(coef(summary.lm(errors.aov))[1:5,], 4)

## ----cf-accs-rfgam-ests-----------------------------------------------
model.tables(errors.aov, type="mean", cterms='model')

## ----cuckoos-caps-----------------------------------------------------
cap8 <- "Length versus breadth, compared between cuckoo eggs laid in
        the different host nests. Axes for the scores are overlaid."
cap9 <- "Plot of length versus breadth of cuckoo eggs, now showing
a boundary line for distinguishing `wren` from `not-wren`,
based on the `lda()` analysis."

## ----cuckoos-lda------------------------------------------------------
cuckoos <- within(DAAG::cuckoos,
    levels(species) <- abbreviate(levels(species), 8))
cuckoos.lda <- MASS::lda(species ~ length + breadth, data=cuckoos)

## ----cuckoo-basic-gph-------------------------------------------------
gph <- xyplot(length ~ breadth, groups=species, data=cuckoos,
              type=c("p"), auto.key=list(space="right"), aspect=1,
              scales=list(tck=0.5), par.settings=simpleTheme(pch=16))

## ----cuckoos-sc, fig.cap=cap8,out.width='70%', fig.width=6.25, fig.height=4.75----
## library(latticeExtra)  # This package has the function layer()
LDmat <- cuckoos.lda$scaling
ld1 <- LDmat[,1]
ld2 <- LDmat[,2]
gm <- sapply(cuckoos[, c("length", "breadth")], mean)
av1 <- gm[1] + ld1[2]/ld1[1]*gm[2]
av2 <- gm[1] + ld2[2]/ld2[1]*gm[2]
addlayer <- latticeExtra::layer(panel.abline(av1, -ld1[2]/ld1[1], lty=1),
                                panel.abline(av2, -ld2[2]/ld2[1], lty=2))
gph + addlayer

## ----calc-scores------------------------------------------------------
gm <- apply(cuckoos.lda$means*cuckoos.lda$prior,2,sum)
  # Grand 'means'
## Sweep out grand 'means'
centered <- sweep(as.matrix(cuckoos[,1:2]), 2, gm)

## ----loo-cv-acc-------------------------------------------------------
## Leave-one-out cross-validation
## Accuracies for linear discriminant analysis
cuckooCV.lda <- MASS::lda(species ~ length + breadth,
                    data=cuckoos, CV=TRUE)
confusion(cuckoos$species, cuckooCV.lda$class,
          gpnames=abbreviate(levels(cuckoos$species), 8))
## Post-multiply by scaling matrix
scores <- centered %*% cuckoos.lda$scaling

## ----qda-loo-cv-acc---------------------------------------------------
## Accuracies for quadratic discriminant analysis
cuckooCV.qda <- MASS::qda(species ~ length + breadth,
                    data=cuckoos, CV=TRUE)
acctab <-confusion(cuckoos$species, cuckooCV.qda$class,
                   gpnames=levels(cuckoos$species),
                   printit=FALSE)$confusion
tab <- table(cuckoos$species)
##
## Overall accuracy
cat("Overall accuracy =",
    sum(diag(acctab)*tab)/sum(tab), "\n")
## Confusion matrix
round(acctab, 3)

## ----CVfalse----------------------------------------------------------
cuckoos.lda <- MASS::lda(species ~ length + breadth, data=cuckoos)

## ----calc-discrim-----------------------------------------------------
x <- pretty(cuckoos$breadth, 20)
y <- pretty(cuckoos$length, 20)
Xcon <- expand.grid(breadth=x, length=y)
cucklda.pr <- predict(cuckoos.lda, Xcon)$posterior

## ----cuckoos-contour, fig.cap=cap9,out.width='70%', fig.width=6.25, fig.height=4.75----
m <- match("wren", colnames(cucklda.pr))
ldadiff <- apply(cucklda.pr, 1, function(x)x[m]-max(x[-m]))
addlayer1 <- latticeExtra::as.layer(contourplot(ldadiff ~ breadth*length,
                                  at=c(-1,0,1), labels=c("", "lda",""),
                                  label.style="flat", col='gray',
                                  data=Xcon), axes=FALSE)
gph + addlayer1

## ----cf-acc-lda-qda---------------------------------------------------
library(gamclass, quietly=TRUE)
cucklda.pr <- cuckooCV.lda$posterior
cucklda.pr2 <- MASS::lda(species ~ length + breadth + I(length^2)
                   + I(breadth^2) + I(length*breadth), CV=TRUE,
                   data=cuckoos)$posterior
compareModels(groups=cuckoos$species,
              estprobs=list(lda=cucklda.pr,
                            "lda plus"=cucklda.pr2),
              gpnames=abbreviate(levels(cuckoos$species),8))

## ----getdata-spam-----------------------------------------------------
data(spam, package='kernlab')
spam[,-58] <- scale(spam[,-58])
nr <- sample(1:nrow(spam))
spam0 <- spam[nr[1:2601],]      ## Training
spam1 <- spam[nr[2602:3601],]   ## Holdout
spam01 <- spam[nr[1:3601],]     ## Use for training,
                                ## if holdout not needed
spam2 <- spam[nr[3602:4601],]   ## Test

## ----lda-rates--------------------------------------------------------
library('MASS')
spam01.lda <- lda(type~., data=spam01)
ldaRates <- ldaErr(train.lda=spam01.lda, train=spam01, test=spam2,
                   group='type')

## ----rpart-dofun------------------------------------------------------
set.seed(29)   ## Make results precisely reproducible
spam01.rp <- rpart::rpart(type~., data=spam01, cp=0.0001)
rpRates <- rpartErr(train.rp=spam01.rp, train=spam01, test=spam2,
                    group='type')

## ----rf-dofun---------------------------------------------------------
set.seed(29)   ## Make results precisely reproducible
spam01.rf <- randomForest::randomForest(type ~ ., data=spam01)
rfRates <- rfErr(train.rf=spam01.rf, train=spam01, test=spam2,
                 group='type')

## ----acctable---------------------------------------------------------
lda_main <- c(round(ldaRates['loo'], 3), round(ldaRates['trainerr'], 3),'-',
              round(ldaRates['testerr'], 3))
rpartAcc <- c('-',round(rpRates['cverror'], 3),round(rpRates['trainerror'], 3),
           round(rpRates['testerror'], 3))
rfAcc <- c('-',round(rfRates['trainerr'], 3),round(rfRates['OOBerr'], 3),
                  round(rfRates['testerr'], 3))
accmat <- rbind(lda_main,rpartAcc,rfAcc)
rownames(accmat) <- c('lda (main effects)','rpart','randomForest')
kable(accmat, col.names=c("Leave One Out CV","Training",
                          "Out of Bag","Test rate"), align=rep('r',4))

## ----lda-x, eval=FALSE------------------------------------------------
#  mm01x <- model.matrix(type ~ 0+.^2, data=spam01)
#  mm2x <- model.matrix(type ~ 0+.^2, data=spam2)
#  spam01x.lda <- lda(x=mm01x, grouping=spam01[,'type'])
#  ldaRatesx <- ldaErr(train.lda=spam01x.lda, train=mm01x, test=mm2x,
#                      group=spam01[,'type'])
#  round(ldaRatesx,3)

## ----errRates, echo=FALSE---------------------------------------------
c(loo=0.291,  trainerr=0.027,  testerr=0.164)

## ----data-Vowel-------------------------------------------------------
data('Vowel', package='mlbench')
Vowel$V1 <-  factor(Vowel$V1)

## ----vowel-qda--------------------------------------------------------
vowelcv.qda <- qda(Class ~ ., data=Vowel, CV=TRUE)
accqda <- confusion(Vowel$Class, vowelcv.qda$class,
                    printit=FALSE)$overall
print(round(accqda,3))
## Compare with CV performance
accspqda <- CVcluster(Class ~ ., id=V1,
                      data=Vowel, nfold=15, FUN=qda,
                      printit=FALSE)$CVaccuracy
print(round(accspqda,3))

## ----vowel-rf---------------------------------------------------------
data('Vowel', package='mlbench')
Vowel$V1 <-  factor(Vowel$V1)
vowel.rf <- randomForest::randomForest(Class ~ ., data=Vowel)
print("OOB accuracy estimate")
accOOB <- 1-sum(rep(1/11,11)*vowel.rf$confusion[,"class.error"])
print(round(accOOB,3))
## Compare with performance based on OOB choice of speakers
accspOOB <- RFcluster(Class ~ ., id=V1,
                      data=Vowel, ntree=500,
                      progress = FALSE)$OOBaccuracy
print(round(accspOOB),3)

