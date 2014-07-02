#'
#'@title Run tests.
#'
testPlotMap.CSV<-function(){
res<-plotMap.CSV(psFile='tstRotFALSE',showMap=TRUE,year=2009,col='legalret',label="legal, retained",
                 rotate=FALSE,blocktype='SUM',plt_blocktype='COARSE',plt_blocklocations=TRUE,
                plt_title=TRUE,plt_stations=TRUE,plt_bars=TRUE,plt_surface=TRUE,plt_reflines=TRUE);
res<-plotMap.CSV(psFile='tstRotTRUE',showMap=TRUE,year=2009,col='legalret',label="legal, retained",
                 rotate=TRUE,elev=80,blocktype='SUM',plt_blocktype='COARSE',plt_blocklocations=TRUE,
                 plt_title=TRUE,plt_stations=FALSE,plt_bars=TRUE,plt_surface=TRUE,plt_reflines=TRUE);
res<-plotMap.CSV(psFile='tstRot160',showMap=TRUE,year=2009,col='legalret',label="legal, retained",
                 rotate=160,elev=40,blocktype='SUM',plt_blocktype='COARSE',plt_blocklocations=TRUE,
                plt_title=TRUE,plt_stations=FALSE,plt_bars=TRUE,plt_surface=TRUE,plt_reflines=TRUE);
}