library(fInstrument)
library(lubridate)
   
dtExpiry <- dmy('01-jan-2011')
dtCalc <- dmy('01-jan-2010')

underlying <- 'IBM'
K<-100

# define derivative
a <- fInstrumentFactory("vanilla", quantity=1,
                  params=list(cp='c', strike=K,
                  dtExpiry=dtExpiry, 
		          underlying=underlying,
                  discountRef='USD.LIBOR', trace=TRUE))

# market data in default environment
base.env <- DataProvider()
setData(base.env, underlying, 'Price', dtCalc, 100)
setData(base.env, underlying, 'DivYield', dtCalc, .02)
setData(base.env, underlying, 'ATMVol', dtCalc, .3)
setData(base.env, 'USD.LIBOR', 'Yield', dtCalc, .02)

# Compute NPV under base scenario
getValue(a, 'Price', dtCalc, base.env)

sce <- t(as.matrix(seq(60, 150, length.out=30)))

# derived data provider with price scenarii
sce.env <- DataProvider(parent=base.env)
setData(sce.env, underlying, 'Price', dtCalc, sce)

# Compute and plot NPV per scenario for underlying spot price
p <- getValue(a, 'Price', dtCalc, sce.env)
plot(sce, p,main='Call Price', type='l', lwd=2, ylab='Price', xlab='Spot')


# Asian option
as <- fInstrumentFactory("asian", quantity=1,
                  params=list(cp='c', strike=K,
                  dtExpiry=dtExpiry, dtStart=dtCalc, dtEnd=dtExpiry,
                  avg=0, 
		          underlying=underlying,
                  discountRef='USD.LIBOR', trace=T))

# Compute NPV under base scenario
getValue(as, 'Price', dtCalc, base.env)

# Standard Barrier option
sb <- fInstrumentFactory("StandardBarrier", quantity=1,
                  params=list(cp='cuo', strike=K,
                  barrier=120, rebate=0,
                  dtExpiry=dtExpiry,
                  underlying=underlying,
                  discountRef='USD.LIBOR', trace=T))

# Compute NPV under base scenario
getValue(sb, 'Price', dtCalc, base.env)

