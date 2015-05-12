\name{geoscalePlot}
\alias{geoscalePlot}
\title{Geological time scale plotting.}
\description{Plots two variables and includes the geological time-scale with a number of possible scales.}
\usage{geoscalePlot(ages,data,units=c("Age","Epoch","Period"),
  tick.scale="myr",boxes="Age",abbrev, cex.age=0.3,cex.ts=0.4,cex.pt=1,
  age.lim=NULL,data.lim=NULL,user.scale,ts.col=TRUE,ts.width=0.3,label,
  vers="ICS2015",no.axis=FALSE,direction="horizontal",erotate,arotate,
  urotate,...)}
\arguments{
  \item{ages}{The first dataset, should be the age variable.}
  \item{data}{The second dataset, should be the data variable.}
  \item{units}{The temporal unit(s) to be included in the timescale, options include: "Eon", "Era", "Period", "Epoch", "Age" and "User". The option "User" is required when including a user-defined timescale. This also requires an object to be assigned to user.scale.}
  \item{tick.scale}{The resolution of the tick marks at the base of the timescale, the default is the same as units. The resolution of the scale can also be chosen by specifiying a value or removed entirely by using "no".}
  \item{boxes}{Option to include grey boxes for individual time bins. Same options as for scale.}
  \item{abbrev}{Option to abbreviate names of geological units in the time scale, options are the same as for units.}	
  \item{cex.age}{Size of the numbers on the scale bar.}
  \item{cex.ts}{Size of the text in the time scale.}
  \item{cex.pt}{Size of the individual points.}
  \item{age.lim}{The temporal range for the plot.}
  \item{data.lim}{The range of data to the plotted.}
  \item{user.scale}{The data object to be used when including a user-defined time scale, requires the option "User" to be included in units. See data(UKzones) as an example of the required data format.}
  \item{ts.col}{Whether to include colours in the timescale.}
  \item{ts.width}{Amount of space taken up by plotting the time scale, value must be between 0 and 1.}
  \item{label}{Label for the data axis.}
  \item{vers}{The version of the time scale to use. Options include: "ICS2015","ICS2014","ICS2013","ICS2012","ICS2010","ICS2009" or "ICS2008".}
  \item{no.axis}{Option to remove the label on the y-axis.}
  \item{direction}{The orientation of the plot, can be either "horizontal" or "vertical".}
  \item{erotate}{A numerical value for the rotation for the Epoch/Series temporal units, default values are 90 when direction='horizontal' and 0 when direction='vertical'.} 
  \item{arotate}{A numerical value for the rotation for the Age/Stage temporal units, default values are 90 when direction='horizontal' and 0 when direction='vertical'.} 
  \item{urotate}{A numerical value for the rotation for the User temporal units, default values are 90 when direction='horizontal' and 0 when direction='vertical'.}	
  \item{...}{Other arguments passed to points.}  
}
\author{Mark A. Bell \email{mark.bell521@gmail.com}}
\examples{

### Basic plots

data(traits)
attach(traits)

## 1. A simple bivariant plot
 geoscalePlot(age,trait)

## 2. Including the entire time scale
 geoscalePlot(age,trait,units=c("Eon","Era","Age","Epoch","Period"),type="l")
 
## 3. Including abbreviations in the time scale
 # (a) For one or more temporal units
  geoscalePlot(age,trait,units=c("Eon","Era","Age","Epoch","Period"),type="l",
   abbrev=c("Age","Epoch"))
 # (b) For all temporal units
  geoscalePlot(age,trait,units=c("Eon","Era","Age","Epoch","Period"),type="l",
   abbrev=c("All"))

## 4. Rotating names in the time scale
  geoscalePlot(age,trait,units=c("Eon","Era","Age","Epoch","Period"),type="l",
   erotate=45)

### Including a user defined timescale

data(traits)
 attach(traits)
  data(biozones)

geoscalePlot(age,trait,boxes="User",user.scale=biozones,type="o",
 units=c("Age","Period","User"),xlim=c(65,95),tick.scale="User")
 
}

\keyword{time-series,plot}
