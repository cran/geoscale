\name{geoscale-package}
\alias{geoscale-package}
\alias{geoscale}
\docType{package}
\title{Geological Time Scale Plotting}
\description{Functions for plotting time-series and distributions data against the international geological time scale.}
\details{
\tabular{ll}{
Package: \tab geoscale\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2015-05-12\cr
License: \tab GPL (>=2)\cr
LazyLoad: \tab no\cr
}

}
\author{Mark A. Bell <mark.bell521@gmail.com>}
\references{

Gradstein, F. M., Ogg, J. M., and Schmitz, M., 2012, A geologic time scale, Boston, USA, Elsevier.

}
\keyword{time-series,plot}
\examples{
data(traits)
attach(traits)

geoscalePlot(age,trait,boxes="Age",ts.col=TRUE,scale="Period",type="l")
    }
