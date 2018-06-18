\name{bronchitis}
\alias{bronchitis}
\docType{data}
\title{Chronic bronchitis in a sample of men in Cardiff}
\description{
  The data consist of observations on three variables
for each of 212 men in a sample of Cardiff enumeration districts.
}
\usage{bronchitis}
\format{A data.frame of 212 obs of 3 variables:
  \describe{
  \item{\code{cig}}{numeric, the number of cigarettes per day}
  \item{\code{poll}}{numeric, the smoke level in the locality}
  \item{\code{r}}{integer, 1= respondent suffered from chronic bronchitis}
  \item{\code{rfac}}{factor, with levels \code{abs} (\code{r}=0), and \code{abs} (\code{r}=0)}
    }
  }
\references{Jones, K. (1975), \emph{A geographical contribution to the aetiology of
                  chronic bronchitis}, Unpublished BSc dissertation,
		University of Southampton. Published in
		Wrigley, N. (1976). \emph{Introduction to the use of
		  logit models in geography}, Geo.Abstracts Ltd, CATMOG
		10, University of East Anglia, Norwich.
		
Murray Aitkin, Brian Francis, John Hinde and Ross Darnell (2009). SMIR: Companion to
  Statistical Modelling in R (SMIR). Oxford University Press.	
		}
\source{This copy of the dataset was copied from version 0.02 of the \pkg{SMIR}
package, which in turn obtained it from Jones (1975).}
\note{See p.224 in SMIR}
\examples{
data(bronchit)
}
\keyword{datasets}