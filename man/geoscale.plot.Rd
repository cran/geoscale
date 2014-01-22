\name{geoscale.plot}
\alias{geoscale.plot}
\title{Geological time scale plotting.}
\description{Plots two variables and includes the geological time-scale with a number of possible scales.}
\usage{geoscale.plot(ages,data,units=c("Age","Epoch","Period"),
  scale="Period",boxes="Age",cex.age=0.3,cex.ts=0.4,cex.pt=1,
  xlim=NULL,ylim=NULL,nscale,ts.col=TRUE,height=0.3,label=NULL,
  vers="ICS2013",...)}
\arguments{
  \item{ages}{The first dataset, should be the age variable.}
  \item{data}{The second dataset, should be the data variable.}
  \item{units}{Which time units to include, options include: "Eon", "Era", "Period","Age" and "Epoch". If nscale is being used, add "Other" to periods.}
  \item{scale}{The resolution of the scale bar at the base of the plot. Options are the same as for periods or "myr" can be used for a million year scale bar.}
  \item{boxes}{Option to include grey boxes for individual time bins. Same options as for scale.}
  \item{cex.age}{Size of the numbers on the scale bar.}
  \item{cex.ts}{Size of the text in the time scale.}
  \item{cex.pt}{Size of the individual points.}
  \item{xlim}{x limit for plotting.}
  \item{ylim}{y limit for plotting.}
  \item{nscale}{User defined timescale, must include "Start","End", "Midpoint" and "Period" fields.}
  \item{ts.col}{Whether to include colours in the timescale.}
  \item{height}{Height of the time scale, value must be between 0 and 1.}
  \item{label}{Label for the y-axis.}
  \item{vers}{The version of the time scale to use. Options include: "ICS2013","ICS2012","ICS2010","ICS2009" or "ICS2008".}
  \item{...}{Other arguments passed to points.}  
}
\author{Mark A. Bell \email{mark.bell521@gmail.com}}
\examples{

data(traits)
attach(traits)

geoscale.plot(age,trait,type="l")

# Including the entire time scale

geoscale.plot(age,trait,units=c("Eon","Era","Age","Epoch","Period"),type="l")

# Plotting a user defined timescale

data(traits)
 attach(traits)
data(biozones)

geoscale.plot(age,trait,boxes="Other",nscale=biozones,
 units=c("Age","Other"),xlim=c(70,80),scale="Other")
}

\keyword{time-series,plot}
