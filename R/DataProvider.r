## DataProvider.R ##
#' A DataProvider is a container of market data, to be used in conjunction with a \code{\linkS4class{fInstrument}} object.
#' A data item is defined as the observation of a phenomenon on an
#' instrument. Therefore, the data item is identified by the instrument
#' being observed, the phenomenon and the observation date.
#' @title A container class for storing market data
#' @author P. Henaff
#' @exportClass DataProvider

setClass('DataProvider', contains=c('environment'), representation=representation())

#' Insert data in a DataProvider object
#' @title insert data into DataProvider objectsetData
#' @param instrument (char) instrument name
#' @param phenomenon (char) phenomenon name
#' @param dtObs (date) observation date
#' @param value (numeric) value
#' @exportMethod setData
#' @rdname setData

setGeneric(name="setData",
           def=function(object, instrument, phenomenon,
             dtObs, value){standardGeneric("setData")})


#' @rdname setData
#' @export

setMethod(f='setData',
          signature=signature('DataProvider'),
          definition=function(object, instrument, phenomenon, dtObs, value){
  key = paste(instrument,'-',phenomenon, sep='')
  ts <- timeSeries(value, dtObs)
  assign(key, ts, pos=object)
})

#' Extract data from a DataProvider object
#' @title getData method
#' @param instrument instrument name
#' @param phenomenon phenomenon name
#' @param dtObs observation date
#' @return a vector of value(s)
#' @rdname getData
#' @exportMethod getData

setGeneric(name="getData",
           def=function(object,instrument, phenomenon,
             dtObs){standardGeneric("getData")})

#' @rdname getData
#' @export

setMethod(f='getData',
          signature=signature('DataProvider'),
          definition=function(object, instrument, phenomenon, dtObs) {
  key = paste(instrument,'-',phenomenon, sep='')
  ts <- get(key, envir=as.environment(object))
  t <- time(ts)
  # find the most recent observation <= dtObs
  indx <- as.numeric(t) <= as.numeric(dtObs)
  tt <- tail(t[indx], n=1)
  as.vector(ts[tt,])
})

#' Constructor
#' @title DataProvider constructor
#' @param parent a back-up DataProvider. If the data is not found in the current provider, the back-up provider is searched.
#' @return an object of type DataProvider
#' @rdname DataProvider
#' @export

DataProvider <- function(parent=NULL) {
  # does not work in 2.12: inheritance broken
  # d <- new(Class="DataProvider", new.env(hash=TRUE, parent=as.environment(parent)))
  # TODO: find better solution, this is said to be "very dangerous"
  d <- new(Class="DataProvider", new.env(hash=TRUE))
  if(is.environment(parent)) {
    parent.env(d) <- parent
  }
  return(d)
}
