#'
#'@title Create a pdf from a set of postscript files.
#'
#'@description Function to create a pdf file from a set of postscript files.
#'
#'@param pdfFile - path to output pdf file
#'@param psFiles - vector of paths to the input postscript files
#'
#'@details ??
#'
#'@export
#'
createPDFfromPS<-function(pdfFile,
                          psFiles,
                          shFile='runPStoPDF.sh'){
    
  cat("Creating pdf file '",pdfFile,"'\n");
  outFile<-paste('"',pdfFile,'.pdf"',sep="");#add extension and surround with double quotes
  inpFiles<-paste('"',psFiles,'"',sep="",collapse=" ");
  
  cmdstr<-'#!/bin/bash + \n';
  cmdstr<-paste(cmdstr,'gs -q -dSAFER -dNOPAUSE -dBATCH -sOutputFile=',outFile,sep=''); #
  cmdstr<-paste(cmdstr," -sDEVICE=pdfwrite -c .setpdfwrite -f ",inpFiles,sep="");
  cat(cmdstr,'\n',file=shFile,sep='');
  system(paste('/bin/bash ',shFile,sep=''));
  file.remove(shFile);
  cat("Finished creating pdf file\n");
}
