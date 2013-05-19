### Standard Barrier Options ##
#' A helper function for creating a \code{\linkS4class{fInstrument}} of type 
#' standard barrier european option. Calculations are performed by the function
#' StandardBarrierOption from Rmetrics.
#'
#' @title Standard Barrier European Option
#' @param q quantity >0 for long position, <0 for short
#' @param params list of parameters that define a european arithmetic asian option:
#' \describe{
#'  \item{typeflag [string]}{'c' (call) or 'p' (put) +
#'  \describe{
#'     \item{di}{down-and-in}
#'     \item{ui}{up-and-in}
#'     \item{do}{down-and-out}
#'     \item{uo}{up-and-out}
#'  }}
#'  \item{strike [numeric]}{strike}
#'  \item{barrier [numeric]}{barrier}
#'  \item{rebate [numeric]}{rebate paid out if the barrier has not been breached}
#'  \item{dtExpiry [timeDate]}{expiry date}
#'  \item{underlying [string]}{name of underlying asset}
#'  \item{discountRef [string]}{name of discount curve}
#'  \item{trace [boolean]}{print trace?}
#' }
#' @return an object of type \code{\linkS4class{fInstrument}}
#'
#' @examples
#' v <- StandardBarrier(q=1, params=list(cp='cuo', strike=100, barrier=120,
#'                      rebate=0, dtExpiry=as.timeDate('01-jan-2011'),
#'                      underlying='IBM', discountRef='USD-LIBOR',
#'                      trace=FALSE))
#' @export

StandardBarrier <- function(q, params) {

cp <- params[["cp"]]
Strike <- params[["strike"]]
dtExpiry = params[["dtExpiry"]]
barrier = params[['barrier']]
rebate = params[['rebate']]
Underlying <- params[['underlying']]
df <- params[['discountRef']]
trace = params[['trace']]

getParams <- function(dtCalc, env) {
  spot <- getData(env, Underlying, 'Price', dtCalc)
  sigma <- getData(env, Underlying, 'ATMVol', dtCalc)
  r <- getData(env, df, 'Yield', dtCalc)
  y <- getData(env, Underlying, 'DivYield', dtCalc)
  Time <- tDiff(dtCalc, dtExpiry)


  list(spot=spot, sigma=sigma, r=r, y=y, Time=Time)
}
  
# Price
getP <- function(dtCalc, env) {
  # compute parameters that are time-dependent
  p <- getParams(dtCalc, env)
  if (trace) {
    print(paste('Calling StandardBarrier with spot=', p$spot,
                'Strike=', Strike, 'Barrier=', barrier,
                'Rebate=', rebate, 'r=', p$r, 'y=', p$y, 'sigma=', p$sigma))
    }

  StandardBarrierOption(TypeFlag=cp, S=p$spot, X=Strike,
                  H=barrier, K=rebate, Time=p$Time, r=p$r,
                  b=p$r-p$y, sigma=p$sigma)@price
}

# Delta by finite difference
getD <- function(dtCalc, env) {
  # compute parameters that are time-dependent
  p <- getParams(dtCalc, env)
  if (trace) {
    print(paste('Calling StandardBarrier with spot=', p$spot,
                'Strike=', Strike, 'Barrier=', barrier,
                'Rebate=', rebate, 'r=', p$r, 'y=', p$y, 'sigma=', p$sigma))
    }
  h <- mean(p$spot)*.001
  pu <-   StandardBarrierOption(TypeFlag=cp, S=p$spot+h, X=Strike,
                  H=barrier, K=rebate, Time=p$Time, r=p$r,
                  b=p$r-p$y, sigma=p$sigma)@price
  pd <-   StandardBarrierOption(TypeFlag=cp, S=p$spot-h, X=Strike,
                  H=barrier, K=rebate, Time=p$Time, r=p$r,
                  b=p$r-p$y, sigma=p$sigma)@price
  (pu-pd)/(2*h)
  }

# Gamma by finite difference
getG <- function(dtCalc, env) {
  # compute parameters that are time-dependent
  p <- getParams(dtCalc, env)
  if (trace) {
    print(paste('Calling StandardBarrier with spot=', p$spot,
                'Strike=', Strike, 'Barrier=', barrier,
                'Rebate=', rebate, 'r=', p$r, 'y=', p$y, 'sigma=', p$sigma))
    }
  h <- mean(p$spot)*.001
  pu <-   StandardBarrierOption(TypeFlag=cp, S=p$spot+h, X=Strike,
                  H=barrier, K=rebate, Time=p$Time, r=p$r,
                  b=p$r-p$y, sigma=p$sigma)@price
  pd <-   StandardBarrierOption(TypeFlag=cp, S=p$spot-h, X=Strike,
                  H=barrier, K=rebate, Time=p$Time, r=p$r,
                  b=p$r-p$y, sigma=p$sigma)@price

  pm <-   StandardBarrierOption(TypeFlag=cp, S=p$spot, X=Strike,
                  H=barrier, K=rebate, Time=p$Time, r=p$r,
                  b=p$r-p$y, sigma=p$sigma)@price
  (pu-2*pm+pd)/(h^2)
  }

# Vega by finite difference
getV <- function(dtCalc, env) {
  # compute parameters that are time-dependent
  p <- getParams(dtCalc, env)
  if (trace) {
    print(paste('Calling StandardBarrier with spot=', p$spot,
                'Strike=', Strike, 'Barrier=', barrier,
                'Rebate=', rebate, 'r=', p$r, 'y=', p$y, 'sigma=', p$sigma))
    }

  dv <- .001
  pu <-   StandardBarrierOption(TypeFlag=cp, S=p$spot, X=Strike,
                  H=barrier, K=rebate, Time=p$Time, r=p$r,
                  b=p$r-p$y, sigma=p$sigma+dv)@price
  pd <-   StandardBarrierOption(TypeFlag=cp, S=p$spot, X=Strike,
                  H=barrier, K=rebate, Time=p$Time, r=p$r,
                  b=p$r-p$y, sigma=p$sigma-dv)@price
  (pu-pd)/(2*dv)
  }

desc <- paste("Standard Barrier", cp, " @ ", Strike,
              "Barrier:", barrier, "Rebate:", rebate,
              "expiry:", dtExpiry)
new(Class="fInstrument", type="StandardBarrier", quantity=q, params=params, p=getP, d=getD, g=getG, v=getV, desc=desc)
}
