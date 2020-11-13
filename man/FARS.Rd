\name{FARS}
\alias{FARS}
\docType{data}
\title{
US fatal road accident data for automobiles, 1998 to 2010
}
\description{
  Data are from the US FARS (Fatality Analysis Recording System) archive
  that is intended to include every accident in which there was at least
  one fatality.  Data are limited to vehicles where the front seat
  passenger seat was occupied.  Values are given for selected variables
  only.
}
\usage{FARS}
\format{
  A data frame with 134332 observations on the following 18 variables.
  \describe{
    \item{\code{caseid}}{a character vector. \dQuote{state:casenum:vnum}}  
    \item{\code{state}}{a numeric vector. See the FARS website for details}
    \item{\code{age}}{a numeric vector; 998=not reported; 999=not known. Cases 
    with \code{age} < 16 have been omitted}
    \item{\code{airbag}}{a numeric vector}
    \item{\code{injury}}{a numeric vector; 4 indicates death.  Blanks, unknown,
    and \dQuote{Died prior to accident} have been omitted}
    \item{\code{Restraint}}{a numeric vector}
    \item{\code{sex}}{1=male, 2=female, 9=unknown}
    \item{\code{inimpact}}{a numeric vector; direction of initial impact.
    Categories 1 to 12 describe clock positions, so that 1,11, and 12
    relate to near frontal impacts; 0 is not a collision; 13: top;
    14: undercarriage. 18, introduced in 2005 has been omitted, as have
    404 values in additional categories for 2010.  99 denotes a missing
    value.}
    \item{\code{modelyr}}{a numeric vector}
    \item{\code{airbagAvail}}{a factor with levels \code{no} \code{yes} \code{NA-code}}
    \item{\code{airbagDeploy}}{a factor with levels \code{no} \code{yes} \code{NA-code}}
    \item{\code{D_injury}}{a numeric vector}
    \item{\code{D_airbagAvail}}{a factor with levels \code{no} \code{yes} \code{NA-code}}
    \item{\code{D_airbagDeploy}}{a factor with levels \code{no} \code{yes} \code{NA-code}}
    \item{\code{D_Restraint}}{a factor with levels \code{no} \code{yes} \code{NA-code}}
    \item{\code{year}}{year of accident}
  }
}
\details{
  Data is for automabiles where the right passenger seat was occupied,
  with one observation for each such passenger.  Observations for vehicles
  where the most harmful event was a fire or explosion or immersion
  or gas inhalation, or where someone fell or jumped from the vehicle,
  are omitted.  Data are limited to vehicle body types 1 to 19,48,49,61,
  or 62.  This excludes large trucks, pickup trucks, vans and buses.
  The 2009 and 2010 data does not include information on whether
  airbags were installed.
}
\source{
\url{http://www-fars.nhtsa.dot.gov/Main/index.aspx}
}
\references{
  \url{https://maths-people.anu.edu.au/~johnm/nzsr/taws.html}

  Olson CM, Cummings P, Rivara FP. 2006. Association of first- and
  second-generation air bags with front occupant death in car crashes: a
  matched cohort study. Am J Epidemiol 164:161-169

  Cummings, P; McKnight, B, 2010. Accounting for vehicle, crash, and occupant
  characteristics in traffic crash studies. Injury Prevention 16: 363-366

  Braver, ER; Shardell, M; Teoh, ER, 2010. \emph{How have changes in air
  bag designs affected frontal crash mortality?} Ann Epidemiol 20:499-510.
}
\note{The papers given as references demonstrate the use of Fatal
  Accident Recording System data to assess the effectiveness of airbags
  (even differences between different types of airbags) and seatbelts.
  Useful results can be obtained by matching driver mortality, with and
  without airbags, to mortality rates for right front seat passengers
  in cars without passenger airbags.
  }
\examples{
data(FARS)
}
\keyword{datasets}
