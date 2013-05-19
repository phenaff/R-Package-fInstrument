\name{Asian}
\alias{Asian}
\title{Asian European Option}
\usage{
  Asian(q, params)
}
\arguments{
  \item{q}{quantity >0 for long position, <0 for short}

  \item{params}{list of parameters that define a european
  arithmetic asian option: \describe{ \item{cp [string]}{c
  (call) or p (put)} \item{strike [numeric]}{strike}
  \item{avg [numeric]}{average so far} \item{dtExpiry
  [timeDate]}{expiry date} \item{dtEnd [timeDate]}{end of
  averaging} \item{dtStart [timeDate]}{start of averaging}
  \item{underlying [string]}{name of underlying asset}
  \item{discountRef [string]}{name of discount curve}
  \item{trace [boolean]}{print trace?} }}
}
\value{
  an object of type \code{\linkS4class{fInstrument}}
}
\description{
  A helper function for creating a
  \code{\linkS4class{fInstrument}} of type arithmetic Asian
  European option. Calculations are performed by the
  function TurnbullWakemanAsianApproxOption from Rmetrics.
}
\examples{
v <- Asian(q=1, params=list(cp='c', strike=100, dtExpiry=as.timeDate('01-jan-2011'),
     dtEnd = as.timeDate('01-jan-2011'),
     dtStart = as.timeDate('01-jan-2010'),
     avg=0.0,
     underlying='IBM', discountRef='USD-LIBOR', trace=FALSE))
}

