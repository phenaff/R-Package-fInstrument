\name{StandardBarrier}
\alias{StandardBarrier}
\title{Standard Barrier European Option}
\usage{
  StandardBarrier(q, params)
}
\arguments{
  \item{q}{quantity >0 for long position, <0 for short}

  \item{params}{list of parameters that define a european
  arithmetic asian option: \describe{ \item{typeflag
  [string]}{'c' (call) or 'p' (put) + \describe{
  \item{di}{down-and-in} \item{ui}{up-and-in}
  \item{do}{down-and-out} \item{uo}{up-and-out} }}
  \item{strike [numeric]}{strike} \item{barrier
  [numeric]}{barrier} \item{rebate [numeric]}{rebate paid
  out if the barrier has not been breached} \item{dtExpiry
  [timeDate]}{expiry date} \item{underlying [string]}{name
  of underlying asset} \item{discountRef [string]}{name of
  discount curve} \item{trace [boolean]}{print trace?} }}
}
\value{
  an object of type \code{\linkS4class{fInstrument}}
}
\description{
  A helper function for creating a
  \code{\linkS4class{fInstrument}} of type standard barrier
  european option. Calculations are performed by the
  function StandardBarrierOption from Rmetrics.
}
\examples{
v <- StandardBarrier(q=1, params=list(cp='cuo', strike=100, barrier=120,
                     rebate=0, dtExpiry=as.timeDate('01-jan-2011'),
                     underlying='IBM', discountRef='USD-LIBOR',
                     trace=FALSE))
}

