#'
#'@title Create a pdf from a vector of postscript files.
#'
#'@description Function to create a pdf file from a vector of postscript file names.
#'
#'@param pdfFile - path to output pdf file
#'@param psFiles - vector of paths to the input postscript files
#'
#'@details
#'This function creates and runs a shell script that uses Ghostscript's gs function.
#'Notes:
#'* The input paths will be quoted internally, so they should not be quoted externally
#'even if they contain spaces.
#'
#'@export
#'
createPDF.fromPS<-function(pdfFile,
                          psFiles=NULL,
                          shFile='runPStoPDF.sh'){
    
  cat("Creating pdf file '",pdfFile,"'\n");
  outFile<-paste('"',pdfFile,'.pdf"',sep="");#add extension and surround with double quotes
  inpFiles<-paste('"',psFiles,'"',sep="",collapse=" ");
  
  cmdstr<-'#!/bin/bash + \n';
  cmdstr<-paste(cmdstr,'gs -q -dSAFER -dNOPAUSE -dBATCH -sOutputFile=',outFile,sep=''); #
  cmdstr<-paste(cmdstr," -sDEVICE=pdfwrite -c .setpdfwrite -f ",inpFiles,sep="");
  cat(cmdstr,'\n',file=shFile,sep='');
  system(paste('/bin/bash ',shFile,sep=''));
#  file.remove(shFile);
  cat("Finished creating pdf file\n");
}

#psFiles<-paste('mapSamplePots',1991:1993,'.ps',sep='')
#createPDF.fromPS("pdftest",psFiles)
