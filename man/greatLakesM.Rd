\name{greatLakesM}
\alias{greatLakesM}
\docType{data}
\title{
Monthly Great Lake heights: 1918 - 2019
}
\description{
  Heights, in meters, are for the lakes Erie,  Michigan/Huron, 
  Ontario and St Clair
}
\usage{data(greatLakesM)}
\format{
  The format is:
 'data.frame':	1212 obs. of  7 variables:
 $ month         : Factor w/ 12 levels "apr","aug","dec",..: 5 4 8 1 9 7 6 2 12 11 ...
 $ year          : int  1918 1918 1918 1918 1918 1918 1918 1918 1918 1918 ...
 $ Superior      : num  183 183 183 183 183 ...
 $ Michigan.Huron: num  177 177 177 177 177 ...
 $ St..Clair     : num  175 175 175 175 175 ...
 $ Erie          : num  174 174 174 174 174 ...
 $ Ontario       : num  74.7 74.7 74.9 75.1 75.1 ...
}
\details{
For more details, go to the website that is the source of the data.
}
\source{
  \url{https://www.lre.usace.army.mil/Missions/Great-Lakes-Information/Great-Lakes-Information-2/Water-Level-Data/}
}
\examples{
data(greatLakesM)
mErie <- ts(greatLakesM[,'Erie'], start=1918, frequency=12)
greatLakes <- aggregate(greatLakesM[,-(1:2)], by=list(greatLakesM$year), 
                        FUN=mean)
names(greatLakes)[1] <- 'year'
## maybe str(greatLakesM)
}
\keyword{datasets}
