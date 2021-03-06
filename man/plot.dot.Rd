% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/plot.dot.R
\name{plot.dot}
\alias{plot.dot}
\title{plot.dot}
\usage{
\method{plot}{dot}(x, scale.range = c(1, 6), time.max = max(x$gam$data$t),
  ...)
}
\arguments{
\item{x}{CanaogramGAM to plot}

\item{scale.range}{Scale for dots (see ggplot2::scale_size_continuous)}

\item{time.max}{Maximum time used for time scale}

\item{...}{<not used>}
}
\value{
A ggplot2 object of the plot
}
\description{
plot.dot
}
\examples{
\dontrun{
images <- read.images('./data/Trial-1/one gree_T00', n = 10)
fit <- fit.gam(images)
plot.dot(fit)
}
}

