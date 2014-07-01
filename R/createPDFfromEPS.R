#'
#'@title Create a pdf from a set of encapsulated postscript files.
#'
#'@description Function to create a pdf file from aset of encapsulated postscript files.
#'
#'@param pdfFile - path to output pdf file
#'@param epsFiles - vector of paths to the input eps files
#'
#'@details On Windows, this function creates a batch file ('runEPStoPDF1bat')
#'that calls the GhostScript executable 'gswin64c' and runs it. The environment
#'variable GhostScript_HOME should point to the top GhostScript folder.\n
#'\n
#'On Mac OSX, ??.
#'
#'@export
#'
createPDFfromEPS<-function(pdfFile,epsFiles){
  cat("Creating pdf file\n");
  outFile<-paste('"',pdfFile,'.pdf"',sep="");#add extension and surround with double quotes
  inpFiles<-paste('"',epsFiles,'"',sep="",collapse=" ");
  cmdstr<-'call "%GhostScript_HOME%\\bin\\gswin64c" -q -dSAFER -dNOPAUSE -dBATCH -dEPSFitPage -sPAPERSIZE=letter -sOutputFile='; #
  cmdstr<-paste(cmdstr,outFile,sep="");
  cmdstr<-paste(cmdstr," -sDEVICE=pdfwrite -c .setpdfwrite -f ",inpFiles,sep="");
  cat(cmdstr,'\n',file='runEPStoPDF1.bat',sep='');
  system('runEPStoPDF1',show.output.on.console=TRUE);
  file.remove('runEPStoPDF1.bat')
  cat("Finished creating pdf file\n");
}
