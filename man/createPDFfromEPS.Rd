\name{createPDFfromEPS}
\alias{createPDFfromEPS}
\title{Create a pdf from a set of encapsulated postscript files.}
\usage{
createPDFfromEPS(pdfFile, epsFiles)
}
\arguments{
  \item{pdfFile}{- path to output pdf file}

  \item{epsFiles}{- vector of paths to the input eps files}
}
\description{
Function to create a pdf file from aset of encapsulated
postscript files.
}
\details{


On Windows, this function creates a batch file
('runEPStoPDF1bat') that calls the GhostScript executable
'gswin64c' and runs it. The environment variable
GhostScript_HOME should point to the top GhostScript
folder.\n \n On Mac OSX, ??.
}

