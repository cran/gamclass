plotFars <-
function (tabDeaths=gamclass::frontDeaths,
          statistics = c("airbagAvail", "airbagDeploy", "restraint"))
{
dchar <- as.character(substitute(tabDeaths))
type <- sub('Deaths','',dchar[length(dchar)])
maint <- paste('With/wo ratio of driver death rates:', 
               switch(type, 'front'="frontal", 'side'="side",
                      'rear'='rear', 'other'='other'))
tabAa <- tabDeaths[[statistics[1]]]
tabAd <- tabDeaths[[statistics[2]]]
tabRe <- tabDeaths[[statistics[3]]]
yrs <- as.numeric(dimnames(tabAa)[[1]])
    df <- data.frame(years = yrs, airbagAvail = tabAa[, 2, 4]/tabAa[,
                                  1, 4], airbagDeploy = tabAd[, 2, 4]/tabAd[, 1, 4], 
                     restraint = tabRe[,2, 4]/tabRe[, 1, 4])
            form <- formula(paste(paste(statistics, collapse="+"), "~ years"))
        gph <- xyplot(form, data = df, xlab='', ylab='Ratio of rates: with/without',
                      par.settings=simpleTheme(pch = c(16,16,1)), type=c('p','g'),
                      auto.key=list(columns=length(statistics)),
                      main=maint)
gph
}
