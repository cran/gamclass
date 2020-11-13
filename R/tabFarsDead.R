tabFarsDead <-
    function (dset=gamclass::FARS, fatal = 4, 
              restrict=expression(age>=16&age<998&inimpact%in%c(11,12,1)),
              statistics = c("airbagAvail", "airbagDeploy",
                                        "Restraint"))
    {
        choose <- eval(restrict, envir=dset)
        dset <- dset[choose,]
        vars2 <- paste("D_", statistics, sep = "")
        yrs <- sort(unique(dset[,"year"]))
        tabAa <- tabAd <- tabRe <- array(0, c(length(yrs), 2, 4),
                                         dimnames = list(years = yrs, D_airbagAvail = levels(dset[,'D_airbagAvail'])[1:2],
                                                         injury = c("P_injury", "D_injury", "tot", "prop")))
        names(dimnames(tabAd)[2]) <- vars2[2]
        names(dimnames(tabRe)[3]) <- vars2[3]
        i <- 0
        for (yr in yrs) {
            i <- i + 1
            yrdat <- subset(dset, dset[,"year"]==yr)
            subdat <- subset(yrdat[, c(vars2[1], "injury", "D_injury")],
                             yrdat[, statistics[1]] == "no")
            if (yr %in% c(2009, 2010))
                tabAa[i, , 1:2] <- NA
            else tabAa[i, , 1:2] <- as.matrix(aggregate(subdat[,
                                                               c("injury", "D_injury")], by = list(subdat[, vars2[1]]),
                                                        FUN = function(x) sum(x %in% fatal))[-3, -1])
            tabAa[i, , 3] <- tabAa[i, , 1] + tabAa[i, , 2]
            tabAa[i, , 4] <- tabAa[i, , 2]/tabAa[i, , 1]
            subdat <- subset(yrdat[, c(vars2[2], "injury", "D_injury")],
                             yrdat[, statistics[2]] == "no")
            tabAd[i, , 1:2] <- as.matrix(aggregate(subdat[, c("injury",
                                                              "D_injury")], by = list(subdat[, vars2[2]]), FUN = function(x) sum(x %in%
                                                                                                                                     fatal))[-3, -1])
            tabAd[i, , 3] <- tabAd[i, , 1] + tabAd[i, , 2]
            tabAd[i, , 4] <- tabAd[i, , 2]/tabAd[i, , 1]
            subdat <- subset(yrdat[, c(vars2[3], "injury", "D_injury")],
                             yrdat[, statistics[3]] == "no")
            tabRe[i, , 1:2] <- as.matrix(aggregate(subdat[, c("injury",
                                                              "D_injury")], by = list(subdat[, vars2[3]]), FUN = function(x) sum(x %in%
                                                                                                                                     fatal))[-3, -1])
            tabRe[i, , 3] <- tabRe[i, , 1] + tabRe[i, , 2]
            tabRe[i, , 4] <- tabRe[i, , 2]/tabRe[i, , 1]
        }
        attr(tabAa,'names') <- attr(tabAd,'names') <- attr(tabRe,'names') <- NULL
        invisible(list(airbagAvail = tabAa, airbagDeploy = tabAd,
                       restraint = tabRe))
    }
function (dset=fars,
          fatal = 4, statistics = c("airbagAvail", "airbagDeploy",
                                    "Restraint"))
{
    vars2 <- paste("D_", statistics, sep = "")
    yrs <- sort(unique(dset[,"year"]))
    tabAa <- tabAd <- tabRe <- array(0, c(length(yrs), 2, 4),
                                     dimnames = list(years = yrs, D_airbagAvail = levels(dset[,'D_airbagAvail'])[1:2],
                                                     injury = c("P_injury", "D_injury", "tot", "prop")))
    names(tabAd)[2] <- vars2[2]
    names(tabRe)[2] <- vars2[3]
    i <- 0
    for (yr in yrs) {
        i <- i + 1
        yrdat <- subset(dset, dset[,"year"]==yr)
        subdat <- subset(yrdat[, c(vars2[1], "injury", "D_injury")],
                         yrdat[, statistics[1]] == "no")
        if (yr %in% c(2009, 2010))
            tabAa[i, , 1:2] <- NA
        else tabAa[i, , 1:2] <- as.matrix(aggregate(subdat[,
                                                           c("injury", "D_injury")], by = list(subdat[, vars2[1]]),
                                                    FUN = function(x) sum(x %in% fatal))[-3, -1])
        tabAa[i, , 3] <- tabAa[i, , 1] + tabAa[i, , 2]
        tabAa[i, , 4] <- tabAa[i, , 2]/tabAa[i, , 1]
        subdat <- subset(yrdat[, c(vars2[2], "injury", "D_injury")],
                         yrdat[, statistics[2]] == "no")
        tabAd[i, , 1:2] <- as.matrix(aggregate(subdat[, c("injury",
                                                          "D_injury")], by = list(subdat[, vars2[2]]), FUN = function(x) sum(x %in%
                                                                                                                                 fatal))[-3, -1])
        tabAd[i, , 3] <- tabAd[i, , 1] + tabAd[i, , 2]
        tabAd[i, , 4] <- tabAd[i, , 2]/tabAd[i, , 1]
        subdat <- subset(yrdat[, c(vars2[3], "injury", "D_injury")],
                         yrdat[, statistics[3]] == "no")
        tabRe[i, , 1:2] <- as.matrix(aggregate(subdat[, c("injury",
                                                          "D_injury")], by = list(subdat[, vars2[3]]), FUN = function(x) sum(x %in%
                                                                                                                                 fatal))[-3, -1])
        tabRe[i, , 3] <- tabRe[i, , 1] + tabRe[i, , 2]
        tabRe[i, , 4] <- tabRe[i, , 2]/tabRe[i, , 1]
    }
    dimnames(tabAa)[[3]]<- dimnames(tabAd)[[3]] <- dimnames(tabRe)[[3]]  <- 
        c("P_deaths", "D_deaths", "tot", "prop")
    invisible(list(airbagAvail = tabAa, airbagDeploy = tabAd,                         
                            restraint = tabRe))        
}
