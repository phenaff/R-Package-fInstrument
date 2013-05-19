#' @include DataProvider.r
NULL
#' @include Vanilla.r
NULL
#' @include Binary.r
NULL
#' @include Asian.r
NULL
#' @include StandardBarrier.r
NULL

#' fInstrument
#'
#' The fInstrument class provides an abstraction layer over
#'  the various types of financial claims available in Rmetrics. The class
#'  exposes methods for computing NPV and greeks. With this class,
#'  you can perform calculations on a portfolio of financial
#'  instruments, without having to be concerned with the implementation
#'  details specific to each kind of instrument.
#'
#' @title A class for representing financial instruments
#'
#' @slot type the type of instrument
#' @slot quantity >0 for long position, <0 for short
#' @slot params list of parameters that define this instrument
#' @slot u function that computes PV01
#' @slot p function that computes NPV
#' @slot d function that computes delta
#' @slot g function that computes gamma
#' @slot v function that computes vega
#' @slot desc a description of the instrument
#'
#' @examples
#' dtExpiry <- as.timeDate('01-jan-2011')
#' underlying <- 'IBM'
#' K<-100
#' a <- fInstrumentFactory("vanilla", quantity=1,
#'                params=list(cp='c', strike=K,
#'                dtExpiry=dtExpiry, 
#'		  underlying=underlying,
#'                discountRef='USD.LIBOR', trace=FALSE))
#' show(a)
#'
#' @author P. Henaff
#' @exportClass fInstrument

setClass(Class="fInstrument", representation=representation(type="character", quantity="numeric", 
params="list", u="ANY", p="ANY", d="ANY", g="ANY", v="ANY", desc="character"))

#' Show
#'
#' Provides a description of the instruments (i.e. the value of the main parameters
#' found in the constructor
#' @title Display a description of the instrument
#' @return A description of the instrument
#' @rdname show
#' @exportMethod show

setMethod(f="show", signature=signature("fInstrument"), def=function(object){
  cat(paste("Instrument", object@desc, "Quantity:", object@quantity))
  cat('\n')
})

#' Invoque a calculation on a financial instrument
#' @title GetValue
#' @param selection The type of calculation ('Value', 'Delta', 'Gamma', 'Vega')
#' @param dtCalc the calculation date
#' @param env the data provider
#' @return a time series of results
#' @exportMethod getValue
#' @rdname getValue

setGeneric(name="getValue", def=function(object,selection, dtCalc, env=NULL){standardGeneric("getValue")})

#' @rdname getValue
#' @export

setMethod(f="getValue", signature=signature("fInstrument"),
   definition=function(object, selection, dtCalc, env=NULL){
    
   res <- NULL
   res <- switch(toupper(selection),
   PRICE = object@p(dtCalc, env), 
   DELTA = object@d(dtCalc, env),
   GAMMA = object@g(dtCalc, env),
   VEGA = object@v(dtCalc, env))
   return(res*object@quantity)
})

#' Factory method for constructing objects of type fInstrument
#' @title fInstrument constructor
#' @param type (char) the instrument type
#' @param quantity (numeric) the position
#' @param params (list) list of parameters specific to the isntrument type
#' @examples
#' a <- fInstrumentFactory("vanilla", quantity=1,
#'                  params=list(cp='c', strike=100,
#'                  dtExpiry=as.timeDate('01-jan-2011'), 
#'                  underlying='IBM',
#'                  discountRef='USD.LIBOR', trace=FALSE))
#' @export

fInstrumentFactory <- function(type, quantity, params){

switch(toupper(type),
  VANILLA = Vanilla(quantity, params),
  BINARY = Binary(quantity, params),
  ASIAN = Asian(quantity, params),     
  STANDARDBARRIER = StandardBarrier(quantity, params)     
)
}

