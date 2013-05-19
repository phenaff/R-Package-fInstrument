### Binary Options ##
#' A helper function for creating a \code{\linkS4class{fInstrument}} of type 
#' binary european option. Calculations are performed by the function
#' CashOrNothingOption from Rmetrics.
#'
#' @title Binary European Option
#' @param q quantity >0 for long position, <0 for short
#' @param params list of parameters that define a vanilla option:
#' \describe{
#'  \item{cp [string]}{c (call) or p (put)}
#'  \item{strike [numeric]}{strike}
#'  \item{dtExpiry [timeDate]}{expiry date}
#'	\item{underlying [string]}{name of underlying asset}
#' 	\item{discountRef [string]}{name of discount curve}
#'	\item{trace [boolean]}{print trace?}
#' }
#' @return an object of type \code{\linkS4class{fInstrument}}
#'
#' @examples
#' v <- Binary(q=1, params=list(cp='c', strike=100, dtExpiry=as.timeDate('01-jan-2011'),
#' underlying='IBM', discountRef='USD-LIBOR', trace=FALSE))
#' @export

Binary <- function(q, params) {

cp <- params[["cp"]]
Strike <- params[["strike"]]
dtExpiry = params[["dtExpiry"]]
Underlying <- params[['underlying']]
df <- params[['discountRef']]
trace = params[['trace']]

# Price
getP <- function(dtCalc, env) {
  # get spot value
  Spot <- getData(env, Underlying, 'Price', dtCalc)
  s <- getData(env, Underlying, 'ATMVol', dtCalc)
  r <- getData(env, df, 'Yield', dtCalc)
  y <- getData(env, Underlying, 'DivYield', dtCalc)
  t <- tDiff(dtCalc, dtExpiry)
  if (trace) {
    print(paste('Calling CashOrNothingOption with Spot=', Spot, 'Strike=', Strike, 't=', t, 'r=', r, 'y=', y, 'sigma=', s))
    }
  CashOrNothingOption(TypeFlag=cp, S=Spot, X=Strike, K=1, Time=t, r=r, b=r-y, sigma=s)@price
}

# Delta by finite difference
getD <- function(dtCalc, env) {
  # get spot value
  Spot <- getData(env, Underlying, 'Price', dtCalc)
  s <- getData(env, Underlying, 'ATMVol', dtCalc)
  r <- getData(env, df, 'Yield', dtCalc)
  y <- getData(env, Underlying, 'DivYield', dtCalc)
  t <- tDiff(dtCalc, dtExpiry)
  if (trace) {
    print(paste('Calling CashOrNothingOption with Spot=', Spot, 'Strike=', Strike, 't=', t, 'r=', r, 'y=', y, 'sigma=', s))
    }
  h <- mean(Spot)*.001
  pu <- CashOrNothingOption(TypeFlag=cp, S=Spot+h, X=Strike, K=1, Time=t, r=r, b=r-y, sigma=s)@price
  pd <- CashOrNothingOption(TypeFlag=cp, S=Spot-h, X=Strike, K=1, Time=t, r=r, b=r-y, sigma=s)@price
  (pu-pd)/(2*h)
  }

# Gamma by finite difference
getG <- function(dtCalc, env) {
  # get spot value
  Spot <- getData(env, Underlying, 'Price', dtCalc)
  s <- getData(env, Underlying, 'ATMVol', dtCalc)
  r <- getData(env, df, 'Yield', dtCalc)
  y <- getData(env, Underlying, 'DivYield', dtCalc)
  t <- tDiff(dtCalc, dtExpiry)
  if (trace) {
    print(paste('Calling CashOrNothingOption with Spot=', Spot, 'Strike=', Strike, 't=', t, 'r=', r, 'y=', y, 'sigma=', s))
    }
  h <- mean(Spot)*.001
  p <- CashOrNothingOption(TypeFlag=cp, S=Spot, X=Strike, K=1, Time=t, r=r, b=r-y, sigma=s)@price
  pu <- CashOrNothingOption(TypeFlag=cp, S=Spot+h, X=Strike, K=1, Time=t, r=r, b=r-y, sigma=s)@price
  pd <- CashOrNothingOption(TypeFlag=cp, S=Spot-h, X=Strike, K=1, Time=t, r=r, b=r-y, sigma=s)@price
  (pu-2*p+pd)/(h^2)
  }

# Vega by finite difference
getV <- function(dtCalc, env) {
  # get spot value
  Spot <- getData(env, Underlying, 'Price', dtCalc)
  s <- getData(env, Underlying, 'ATMVol', dtCalc)
  r <- getData(env, df, 'Yield', dtCalc)
  y <- getData(env, Underlying, 'DivYield', dtCalc)
  t <- tDiff(dtCalc, dtExpiry)
  if (trace) {
    print(paste('Calling CashOrNothingOption with Spot=', Spot, 'Strike=', Strike, 't=', t, 'r=', r, 'b=', b, 'sigma=', s))
    }
  dv <- .001
  pu <- CashOrNothingOption(TypeFlag=cp, S=Spot, X=Strike, K=1, Time=t, r=r, b=r-y, sigma=s+dv)@price
  pd <- CashOrNothingOption(TypeFlag=cp, S=Spot, X=Strike, K=1, Time=t, r=r, b=r-y, sigma=s-dv)@price
  (pu-pd)/(2*dv)
  }

desc <- paste("Binary ", cp, " @ ", Strike, " expiry: ", dtExpiry)
new(Class="fInstrument", type="Binary", quantity=q, params=params, p=getP, d=getD, g=getG, v=getV, desc=desc)
}
