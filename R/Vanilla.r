### Vanilla European Options ##
#' A helper function for creating a \code{\linkS4class{fInstrument}} of type vanilla 
#' european option. Calculations are performed by the function GBSOption and GBSGreeks from
#' Rmetrics.
#'
#' @title Vanilla European Option
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
#' v <- Vanilla(q=1, params=list(cp='c', strike=100, dtExpiry=as.timeDate('01-jan-2011'),
#' underlying='IBM', discountRef='USD-LIBOR', trace=FALSE))
#' @export

Vanilla <- function(q, params) {

cp <- params[["cp"]]
Strike <- params[["strike"]]
dtExpiry = params[["dtExpiry"]]
Underlying <- params[['underlying']]
df <- params[['discountRef']]
trace = params[['trace']]

getP <- function(dtCalc, env) {
  # get spot value
  Spot <- getData(env, Underlying, 'Price', dtCalc)
  s <- getData(env, Underlying, 'ATMVol', dtCalc)
  r <- getData(env, df, 'Yield', dtCalc)
  y <- getData(env, Underlying, 'DivYield', dtCalc)
  t <- tDiff(dtCalc, dtExpiry)
  if (trace) {
    print(paste('Calling GBSOption with Spot=', Spot, 'Strike=', Strike, 't=', t, 'r=', r, 'y=', y, 'sigma=', s))
    }
  GBSOption(TypeFlag=cp, S=Spot, X=Strike, Time=t, r=r, b=r-y, sigma=s)@price
}

getD <- function(dtCalc, env) {
  Spot <- getData(env, Underlying, 'Price', dtCalc)
  s <- getData(env, Underlying, 'ATMVol', dtCalc)
  r <- getData(env, df, 'Yield', dtCalc)
  y <- getData(env, Underlying, 'DivYield', dtCalc)
  t <- tDiff(dtCalc, dtExpiry)
  GBSGreeks(Selection="delta", TypeFlag=cp, S=Spot, X=Strike, Time=t, r=r, b=r-y, sigma=s)
}

getG <- function(dtCalc, env) {
  Spot <- getData(env, Underlying, 'Price', dtCalc)
  s <- getData(env, Underlying, 'ATMVol', dtCalc)
  r <- getData(env, df, 'Yield', dtCalc)
  y <- getData(env, Underlying, 'DivYield', dtCalc)
  t <- tDiff(dtCalc, dtExpiry)
  GBSGreeks(Selection="gamma", TypeFlag=cp, S=Spot, X=Strike, Time=t, r=r, b=r-y, sigma=s)
}

getV <- function(dtCalc, env) {
  Spot <- getData(env, Underlying, 'Price', dtCalc)
  s <- getData(env, Underlying, 'ATMVol', dtCalc)
  r <- getData(env, df, 'Yield', dtCalc)
  y <- getData(env, Underlying, 'DivYield', dtCalc)
  t <- tDiff(dtCalc, dtExpiry)
  GBSGreeks(Selection="vega", TypeFlag=cp, S=Spot, X=Strike, Time=t, r=r, b=r-y, sigma=s)
}

desc <- paste("Vanilla ", Underlying, cp, " @ ", Strike, " expiry: ", dtExpiry)
new(Class="fInstrument", type="Vanilla", quantity=q, params=params, p=getP, d=getD, g=getG, v=getV, desc=desc)
}

