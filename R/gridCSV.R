#'
#'@title Grid a dataframe (or csv file) using GMT.
#'
#'@description Function to grid a dataframe or csv file using GMT.
#'
#'@param dfr - the dataframe to grid
#'@param csvin - the path to the csv file to grid
#'@param lat - column name containing latitude coordinates
#'@param lon - column name containing longitude coordinates
#'@param col - column with value to grid
#'@param xyrng - GMT range string for grid limits, e.g. 'minLon/maxLon/minLat/maxLat'
#'@param delx - grid cell size in x (longitude)
#'@param dely - grid cell size in y (latitude)
#'@param blocktype - blocking type for values falling inside a grid cell ('SUM' or 'MEAN')
#'@param tmpFile - temporary csv file name 
#'@param tmpData - temporary data file name
#'@param tmpGrid - temporary grid file name
#'
#'@return dataframe with the gridded values
#'
#'@importFrom wtsUtilities getOperatingSystem
#'
#'@export
#'
gridCSV<-function(dfr=NULL,
                  csvin=NULL,
                  lat='latitude',
                  lon='longitude',
                  col='legalret',
                  xyrng='180/205/54/62',
                  delx=0.4,
                  dely=0.2,
                  blocktype='SUM',
                  tmpFile=file.path(getwd(),'tmpFile.csv'),
                  tmpData=file.path(getwd(),'tmpData.txt'),
                  tmpGrid=file.path(getwd(),'tmpGrid.xyg')){
  
    #check the operating platform
    MacOSX<-'MacOSX';
    Windws<-'Windows';
    platform<-wtsUtilities::getOperatingSystem();
    
    #check for unc paths
    if (platform=='Windows'){
        if (length(grep('\\\\',getwd()))>0){
            cat("Working dir is using a network path:\n",
                getwd(),'\n',
                'This function uses Windows .bat files.\n',
                'Please use a mapped drive path for the working directory!!\n',
                'Exiting function\n');
            return;
        }
    }
  
    if (is.null(dfr)){
        #read in table from csv file
        if (is.null(csvin)) {
            dfr = wtsUtilities::getCSV(caption="Select csv file to grid");
            if (is.null(dfr)) return(NULL);
        } else {
            cat('\nRead csv file "',csvin,'"\n\n');
            dfr<-read.csv(csvin,stringsAsFactors=FALSE);
        }
        names(dfr)<-tolower(names(dfr));
    }
    
    #extract only required columns and adjust to positive longitudes
    dfr<-dfr[,c(lon,lat,col)];
    dfr[[lon]]<-dfr[[lon]]*(dfr[[lon]]>0) + (360+dfr[[lon]])*(dfr[[lon]]<0);
    
    
    #save to temporary file csvfile 
    write.table(dfr,
              tmpFile,
              sep=',',
              na="",
              quote=FALSE,
              col.names=FALSE,
              row.names=FALSE)
    
    zscale<-max(dfr[,3],na.rm=TRUE);
  
    if (blocktype=='SUM')  blktyp<-'-Sz';
    if (blocktype=='MEAN') blktyp='';
    
    if (platform==MacOSX){shll<-'#!/bin/sh -x'; batfile<-'gridCSV.sh';  set<-'export ';}
    if (platform==Windws){shll<-'';             batfile<-'gridCSV.bat'; set<-'set ';}
    
    if (platform==MacOSX){blockmean<-paste('gmt blockmean ',tmpData,' -I${delx}/${dely} ${blktyp} ${range} -C -F -V 1> ',sep='');}
    if (platform==Windws){blockmean<-paste('    blockmean ',tmpData,' -I%delx%/%dely%   %blktyp%  %range%  -C -F -V 1> ',sep='');}
    
    setenvs<-paste(set,'delx=',delx,'\n',
                   set,'dely=',dely,'\n',
                   set,'blktyp=',blktyp,'\n',
                   set,'zscale=',zscale,'\n',
                   set,'range=-R',xyrng,sep='');
    
    cat(shll,'\n',setenvs,'\n',
        "gawk -F, '{print $1, $2, $3}' ",tmpFile,' 1> ',tmpData,' \n',
         blockmean,tmpGrid,' \n',
         file=batfile,sep='');
    
    if (platform==MacOSX) Sys.chmod(batfile);#make it executable
    
    if (platform==MacOSX) system2(paste('./',batfile,sep=''));
    if (platform==Windws) system(batfile,show.output.on.console=TRUE);
    
    dfr1<-read.csv(tmpGrid,header=FALSE,sep="")
    names(dfr1)<-c(lon,lat,col);
    
    file.remove(batfile)
    file.remove(tmpFile)
    file.remove(tmpData)
    file.remove(tmpGrid)
    
    return(dfr1)
}