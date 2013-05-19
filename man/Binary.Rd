\name{Binary}
\alias{Binary}
\title{Binary European Option}
\usage{
  Binary(q, params)
}
\arguments{
  \item{q}{quantity >0 for long position, <0 for short}

  \item{params}{list of parameters that define a vanilla
  option: \describe{ \item{cp [string]}{c (call) or p
  (put)} \item{strike [numeric]}{strike} \item{dtExpiry
  [timeDate]}{expiry date} \item{underlying [string]}{name
  of underlying asset} \item{discountRef [string]}{name of
  discount curve} \item{trace [boolean]}{print trace?} }}
}
\value{
  an object of type \code{\linkS4class{fInstrument}}
}
\description{
  A helper function for creating a
  \code{\linkS4class{fInstrument}} of type binary european
  option. Calculations are performed by the function
  CashOrNothingOption from Rmetrics.
}
\examples{
v <- Binary(q=1, params=list(cp='c', strike=100, dtExpiry=as.timeDate('01-jan-2011'),
underlying='IBM', discountRef='USD-LIBOR', trace=FALSE))
}

