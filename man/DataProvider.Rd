\name{DataProvider}
\alias{DataProvider}
\title{A container class for storing market data}
\usage{
  DataProvider(parent = NULL)
}
\arguments{
  \item{parent}{a back-up DataProvider. If the data is not
  found in the current provider, the back-up provider is
  searched.}
}
\value{
  an object of type DataProvider
}
\description{
  A DataProvider is a container of market data, to be used
  in conjunction with a \code{\linkS4class{fInstrument}}
  object. A data item is defined as the observation of a
  phenomenon on an instrument. Therefore, the data item is
  identified by the instrument being observed, the
  phenomenon and the observation date.

  Constructor
}
\author{
  P. Henaff
}

