% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/plot.summary.R
\name{plot.summary}
\alias{plot.summary}
\title{plot.summary}
\usage{
\method{plot}{summary}(x, file = paste0(x$root, ".pdf"), scale = NA, ...)
}
\arguments{
\item{x}{CanaogramGAM to plot}

\item{file}{Output file to save plot to}

\item{scale}{Scale at which to plot dot-plots}

\item{...}{<not used>}
}
\value{
A ggplot2 object of the plot
}
\description{
plot.summary
}
\examples{
\dontrun{
images <- read.images('./data/Trial-1/one gree_T00')
fit <- fit.gam(images)
plot.summary(fit)
}
}

