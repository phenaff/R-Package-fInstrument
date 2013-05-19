\name{fInstrument}
\alias{fInstrument}
\title{A class for representing financial instruments}
\description{
  fInstrument
}
\details{
  The fInstrument class provides an abstraction layer over
  the various types of financial claims available in
  Rmetrics. The class exposes methods for computing NPV and
  greeks. With this class, you can perform calculations on
  a portfolio of financial instruments, without having to
  be concerned with the implementation details specific to
  each kind of instrument.
}
\examples{
dtExpiry <- as.timeDate('01-jan-2011')
underlying <- 'IBM'
K<-100
a <- fInstrumentFactory("vanilla", quantity=1,
               params=list(cp='c', strike=K,
               dtExpiry=dtExpiry,
		  underlying=underlying,
               discountRef='USD.LIBOR', trace=FALSE))
show(a)
}
\author{
  P. Henaff
}

