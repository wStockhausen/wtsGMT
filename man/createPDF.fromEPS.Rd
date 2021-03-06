% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createPDF.fromEPS.R
\name{createPDF.fromEPS}
\alias{createPDF.fromEPS}
\title{Create a pdf from a set of encapsulated postscript files.}
\usage{
createPDF.fromEPS(pdfFile, epsFiles)
}
\arguments{
\item{pdfFile}{- path to output pdf file}

\item{epsFiles}{- vector of paths to the input eps files}
}
\description{
Function to create a pdf file from aset of encapsulated postscript files.
}
\details{
* On Windows, this function creates a batch file ('runEPStoPDF1bat')
that calls the GhostScript executable 'gswin64c' and runs it. The environment
variable GhostScript_HOME should point to the top GhostScript folder.

* On Mac OSX, ??.
}

