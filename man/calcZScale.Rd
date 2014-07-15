\name{calcZScale}
\alias{calcZScale}
\title{Calculate a z scale for plotting data on a map.}
\usage{
calcZScale(z, logtr = FALSE, satfac = 1.1)
}
\arguments{
  \item{z}{- data vector to calculate scale from}

  \item{logtr}{- flag (T/F) to ln-transform z before
  calculating the scale}

  \item{satfac}{- saturation factor relative to maximum}
}
\value{
the scale to be used for plotting z
}
\description{
Function to calculate a z scale for plotting data on a map.
}
\details{

}

