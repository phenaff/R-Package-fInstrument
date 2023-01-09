### Asian Options ##
#' A helper function for creating a \code{\linkS4class{fInstrument}} of type arithmetic asian european option.
#' Calculations are performed by the function TurnbullWakemanAsianApproxOption from Rmetrics. 
#'
#' @title Asian European Option
#' @param q quantity >0 for long position, <0 for short
#' @param params list of parameters that define a european arithmetic asian option:
#' \describe{
#'  \item{cp [string]}{c (call) or p (put)}
#'  \item{strike [numeric]}{strike}
#'  \item{avg [numeric]}{average so far}
#'  \item{dtExpiry [timeDate]}{expiry date}
#'  \item{dtEnd [timeDate]}{end of averaging}
#'  \item{dtStart [timeDate]}{start of averaging}
#'  \item{underlying [string]}{name of underlying asset}
#'  \item{discountRef [string]}{name of discount curve}
#'  \item{trace [boolean]}{print trace?}
#' }
#' @return an object of type \code{\linkS4class{fInstrument}}
#'
#' @examples 
#' a <- fInstrumentFactory("asian", quantity=1,
#'                  params=list(cp='c', strike=100,
#'                  dtExpiry=dmy('01-jan-2011'), 
#'                  underlying='IBM',
#'                  discountRef='USD.LIBOR', trace=FALSE))

#' @export

library(timeDate)

Asian <- function(q, params) {

cp <- params[["cp"]]
Strike <- params[["strike"]]
dtExpiry = params[["dtExpiry"]]
dtStart = params[["dtStart"]]
dtEnd = params[["dtEnd"]]
avg = params[["avg"]]
Underlying <- params[['underlying']]
df <- params[['discountRef']]
trace = params[['trace']]

getParams <- function(dtCalc, env) {
  spot <- getData(env, Underlying, 'Price', dtCalc)
  sigma <- getData(env, Underlying, 'ATMVol', dtCalc)
  r <- getData(env, df, 'Yield', dtCalc)
  b <- getData(env, Underlying, 'DivYield', dtCalc)
  Time <- tDiff(dtCalc, dtExpiry)
  time <- tDiff(dtCalc, dtEnd)
  tau <- tDiff(dtCalc, dtStart)

  list(spot=spot, sigma=sigma, r=r, b=b, Time=Time, time=time, tau=tau)
}
  
# Price
getP <- function(dtCalc, env) {
  # compute parameters that are time-dependent
  p <- getParams(dtCalc, env)
  if (trace) {
    print(paste('Calling TurnbullWakemanAsianApproxOption with ',
                'c/p=', cp, 'spot=', 
                p$spot, 'Strike=', Strike, 'Time=', p$Time, 
                'time=', p$time, 'tau=', p$tau, 'r=', p$r, 
                'b=', p$b, 'sigma=', p$sigma))
    }

  TurnbullWakemanAsianApproxOption(TypeFlag=cp, S=p$spot, SA=avg, X=Strike,
                                   Time=p$Time, time=p$time, tau=p$tau, r=p$r,
                                   b=p$b, sigma=p$sigma)@price
}

# Delta by finite difference
getD <- function(dtCalc, env) {
  # compute parameters that are time-dependent
  p <- getParams(dtCalc, env)
  if (trace) {
    print(paste('Calling TurnbullWakemanAsianApproxOption with spot=', p$spot, 'Strike=', p$strike, 'Time=', p$Time, 'time=', p$time, 'tau=', p$tau, 'r=', p$r, 'b=', p$b, 'sigma=', p$sigma))
    }
  h <- mean(p$spot)*.001
  pu <- TurnbullWakemanAsianApproxOption(TypeFlag=cp, S=p$spot+h, SA=avg, X=Strike,
                                   Time=p$Time, time=p$time, tau=p$tau, r=p$r,
                                   b=p$b, sigma=p$sigma)@price
  pd <- TurnbullWakemanAsianApproxOption(TypeFlag=cp, S=p$spot-h, SA=avg, X=Strike,
                                   Time=p$Time, time=p$time, tau=p$tau, r=p$r,
                                   b=p$b, sigma=p$sigma)@price
  (pu-pd)/(2*h)
  }

# Gamma by finite difference
getG <- function(dtCalc, env) {
  # compute parameters that are time-dependent
  p <- getParams(dtCalc, env)
  if (trace) {
    print(paste('Calling TurnbullWakemanAsianApproxOption with spot=', p$spot, 'Strike=', p$strike, 'Time=', p$Time, 'time=', p$time, 'tau=', p$tau, 'r=', p$r, 'b=', p$b, 'sigma=', p$sigma))
    }
  h <- mean(p$spot)*.001

  pu <- TurnbullWakemanAsianApproxOption(TypeFlag=cp, S=p$spot+h, SA=avg, X=Strike,
                                   Time=p$Time, time=p$time, tau=p$tau, r=p$r,
                                   b=p$b, sigma=p$sigma)@price
  pd <- TurnbullWakemanAsianApproxOption(TypeFlag=cp, S=p$spot-h, SA=avg, X=Strike,
                                   Time=p$Time, time=p$time, tau=p$tau, r=p$r,
                                   b=p$b, sigma=p$sigma)@price
  pm <- TurnbullWakemanAsianApproxOption(TypeFlag=cp, S=p$spot, SA=avg, X=Strike,
                                   Time=p$Time, time=p$time, tau=p$tau, r=p$r,
                                   b=p$b, sigma=p$sigma)@price
  (pu-2*pm+pd)/(h^2)
  }

# Vega by finite difference
getV <- function(dtCalc, env) {
  # compute parameters that are time-dependent
  p <- getParams(dtCalc, env)
  if (trace) {
    print(paste('Calling TurnbullWakemanAsianApproxOption with spot=', p$spot, 'Strike=', p$strike, 'Time=', p$Time, 'time=', p$time, 'tau=', p$tau, 'r=', p$r, 'b=', p$b, 'sigma=', p$sigma))
    }

  dv <- .001
  pu <- TurnbullWakemanAsianApproxOption(TypeFlag=cp, S=p$spot, SA=avg, X=Strike,
                                   Time=p$Time, time=p$time, tau=p$tau, r=p$r,
                                   b=p$b, sigma=p$sigma+dv)@price
  pd <- TurnbullWakemanAsianApproxOption(TypeFlag=cp, S=p$spot, SA=avg, X=Strike,
                                   Time=p$Time, time=p$time, tau=p$tau, r=p$r,
                                   b=p$b, sigma=p$sigma-dv)@price
  (pu-pd)/(2*dv)
  }

desc <- paste("Asian", cp, " @ ", Strike, " expiry: ", dtExpiry)
new(Class="fInstrument", type="Asian", quantity=q, params=params, p=getP, d=getD, g=getG, v=getV, desc=desc)
}
