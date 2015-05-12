\name{geoscaleBox}
\alias{geoscaleBox}
\title{Geological box plots.}
\description{Plots a series of box plots against the geological time-scale.}
\usage{geoscaleBox(data, ages, units = c("Age", "Epoch", "Period"), 
	tick.scale = "myr", boxes = "Age", abbrev, cex.age = 0.3, cex.ts = 0.4, 
	cex.pt = 1, age.lim= NULL, data.lim = NULL, box.width=1, user.scale, 
	ts.col = TRUE, ts.width = 0.3, label,vers="ICS2015",no.axis=FALSE,
	notch=FALSE,log=FALSE, color,direction ="horizontal",erotate,arotate,urotate,...)}
\arguments{
  \item{data}{The distributions to be plotted, either a matrix of columns or as a list (see examples below).}
  \item{ages}{The ages in millions of years for each box to be plotted.}
  \item{units}{The temporal unit(s) to be included in the timescale, options include: "Eon", "Era", "Period", "Epoch", "Age" and "User". The option "User" is required when including a user-defined timescale. This also requires an object to be assigned to user.scale.}
  \item{tick.scale}{The resolution of the tick marks at the base of the timescale, the default is the same as units. The resolution of the scale can also be chosen by specifiying a value or removed entirely by using "no".}
  \item{boxes}{Option to include grey boxes for individual time bins. Same options as for scale.}
  \item{abbrev}{Option to abbreviate names of geological units in the time scale, options are the same as for units.}	
  \item{cex.age}{Size of the numbers on the scale bar.}
  \item{cex.ts}{Size of the text in the time scale.}
  \item{cex.pt}{Size of the individual points.}
  \item{age.lim}{The range of values for the x-axis.}
  \item{data.lim}{The range of values for the y-axis.}
  \item{box.width}{The width of the boxes in millions of years.}
  \item{user.scale}{The data object to be used when including a user-defined time scale, requires the option "User" to be included in units. See data(UKzones) as an example of the required data format.}
  \item{ts.col}{Whether to include colours in the timescale.}
  \item{ts.width}{Proportion of the plot used for the time scale, value must be between 0 and 1.}
  \item{label}{Label for the y-axis.}
  \item{vers}{The version of the time scale to use. Options include: "ICS2015","ICS2014","ICS2013","ICS2012","ICS2010","ICS2009" or "ICS2008".}
  \item{no.axis}{Option to remove the label on the y-axis.}
  \item{notch}{Option to draw notches in the individual boxes, see help(boxplot).}
  \item{log}{Option to plot the y-axis on a log scale, default is FALSE.}
  \item{color}{Option for the color of the boxes.}
  \item{direction}{The orientation of the plot, can be either "horizontal" or "vertical".}
  \item{erotate}{A numerical value for the rotation for the Epoch/Series temporal units, default values are 0 when direction = "upwards" and 90 when direction = "rightwards".} 
  \item{arotate}{A numerical value for the rotation for the Age/Stage temporal units, default values are 0 when direction = "upwards" and 90 when direction = "rightwards".} 
  \item{urotate}{A numerical value for the rotation for the User temporal units, default values are 0 when direction = "upwards" and 90 when direction = "rightwards".}	
  \item{...}{All other options passed to points.}
}
\author{Mark A. Bell \email{mark.bell521@gmail.com}}
\examples{

## Plotting data from a matrix

 data1 <- matrix(ncol=10,nrow=30,data=runif(30,0,100))
  ages <- seq(10,100,10)
    geoscaleBox(data1,ages,boxes="Age",pch=19,box.width=2)    

## Plotting data from a list
 
 data2 <- vector("list",10)
  for(d in 1:length(data2)){
  	data2[[d]] <- runif(30,0,100)
  }
	ages <- seq(10,100,10)
     geoscaleBox(data2,ages,boxes="Age",pch=19,box.width=2)    

## Example dataset

 data(size)
  ages <- as.numeric(colnames(size))
   geoscaleBox(data=size,ages=ages,pch=19,boxes="Age")

# Plotting on a log scale
   geoscaleBox(data=size,ages=ages,pch=19,boxes="Age",log=TRUE)

}

\keyword{time-series,plot}
