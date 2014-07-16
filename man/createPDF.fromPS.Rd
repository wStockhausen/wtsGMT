\name{createPDF.fromPS}
\alias{createPDF.fromPS}
\title{Create a pdf from a vector of postscript files.}
\usage{
createPDF.fromPS(pdfFile, psFiles = NULL, shFile = "runPStoPDF.sh")
}
\arguments{
  \item{pdfFile}{- path to output pdf file}

  \item{psFiles}{- vector of paths to the input postscript
  files}
}
\description{
Function to create a pdf file from a vector of postscript
file names.
}
\details{


This function creates and runs a shell script that uses
Ghostscript's gs function. Notes: * The input paths will be
quoted internally, so they should not be quoted externally
even if they contain spaces.
}

