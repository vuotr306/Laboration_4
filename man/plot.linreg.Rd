% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/dokumentation.R
\name{plot.linreg}
\alias{plot.linreg}
\title{Plot a linreg object}
\usage{
\method{plot}{linreg}(x)
}
\arguments{
\item{x}{A linreg object.}
}
\value{
Two residual plots.
}
\description{
A method that creates two plots for a linreg object.
}
\details{
Two plots are made where residuals are plotted against fitted values, using the ggplot() function of the \link{ggplot2} library.
The first plot contains a dotted line for the mean value of the residuals and a regression line for the residuals regressed on the fitted values.
The second plot has the square root of the absolute value of the residuals as y-axis variable. Here too, there is a regression line fitted for the y-axis variable regressed on the fitted values.
}
\examples{
a <- linreg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
plot(a)
}
\seealso{
\link{plot}. The generic function from which plot.linreg is created.
}

