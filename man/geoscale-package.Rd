\name{geoscale-package}
\alias{geoscale-package}
\alias{geoscale}
\docType{package}
\title{Geological time scale plot.}
\description{A function for plotting time-series data against the current ICS geological timescale.}
\details{
\tabular{ll}{
Package: \tab geoscale\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-05-13\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}

}
\author{Mark A. Bell <mark.bell521@gmail.com>}
\references{

Gradstein, F. M., Ogg, J. M., and Schmitz, M., 2012, A geologic time scale, Boston, USA, Elsevier.

}
\keyword{time series}
\examples{
data(traits)
attach(traits)

geoscale.plot(age,trait,boxes="Age",ts.col=TRUE,scale="Period",type="l")
    }
