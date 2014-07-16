#'
#'@title Grid a dataframe (or csv file) using GMT 4.5.
#'
#'@description Function to grid a dataframe or csv file using GMT 4.5.
#'
#'@details
#'Input dataframe is processed using GMT 4.5's 'blockmean' function using center coordinates for the
#'output blocks as intermediate locations. These coordinates are then shifted by 0.5*cell width
#'in the x and y directions so that the cell locations correspond to a pixel-registered grid. Converting
#'the result to a grid using GMT 4.5's xyz2grd with pixel registration turned on (using -F) results
#'in the expected behavior.
#'
#'* note that an intermediate file 'tmp.xyg' is created and deleted during this process
#'
#'@param dfr - the dataframe to grid
#'@param lat - column name containing latitude coordinates
#'@param lon - column name containing longitude coordinates
#'@param col - column with value to grid
#'@param logtr - flag (T/F) to log10-transform z values before gridding
#'@param xyrng - GMT range string for grid limits, e.g. 'minLon/maxLon/minLat/maxLat'
#'@param delx - grid cell size in x (longitude)
#'@param dely - grid cell size in y (latitude)
#'@param blocktype - blocking type for values falling inside a grid cell ('SUM' or 'MEAN')
#'@param xyzFile - temporary file name for xyz values
#'@param tmpGrid - temporary file name for grid values
#'@param cleanup - flag (T/F) to remove temporary files
#'
#'@return dataframe with the gridded values as column names 'lon','lat', and col.
#'
#'@importFrom wtsUtilities getOperatingSystem
#'@importFrom wtsUtilities getCSV
#'
#'@export
#'
gridCSV<-function(dfr=NULL,
                  lat='latitude',
                  lon='longitude',
                  col=NULL,
                  logtr=FALSE,
                  xyrng='180/205/54/62',
                  delx=0.50,
                  dely=0.25,
                  blocktype=c('MEAN','SUM'),
                  xyzFile=file.path(getwd(),'tmpXYZ.txt'),
                  tmpGrid=file.path(getwd(),'tmpGrid.xyg'),
                  cleanup=FALSE){
  
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
  
    if (!is.data.frame(dfr)){
        #read in table from csv file
        if (is.null(dfr)) {
            dfr = wtsUtilities::getCSV(caption="Select csv file to grid");
            if (is.null(dfr)) return(NULL);
        } else {
            cat('\nRead csv file "',dfr,'"\n\n');
            dfr<-read.csv(dfr,stringsAsFactors=FALSE);
        }
        names(dfr)<-tolower(names(dfr));
    }
    
    #extract only required columns and adjust to positive longitudes
    dfr1<-data.frame(lon=dfr[[lon]],lat=dfr[[lat]]);
    dfr1[["lon"]]<-dfr1$lon*(dfr1$lon>0) + (360+dfr1$lon)*(dfr1$lon<0);
    dfr1[["z1"]] <-dfr[[col]];
    
    if (logtr){
        dfr1[["z1"]]<-log(dfr1$z1+1)/log(10);#z log10 transformed (not ln)
    }
    dfr1[["z2"]]<-dfr1[["z1"]];
    
    #save to temporary xyz file 
    write.table(dfr1,
                xyzFile,
                sep='  ',
                na="",
                quote=FALSE,
                col.names=FALSE,
                row.names=FALSE)
  
    if (blocktype[1]=='SUM')  blktyp<-'-Sz';
    if (blocktype[1]=='MEAN') blktyp='';
    
    rngxy<-paste("-R",xyrng,sep='');
    delxy<-paste(delx,"/",dely,sep='');
    
    if (platform==MacOSX){shll<-'#!/bin/sh +'; batfile<-'gridCSV.sh';  set<-'export ';}
    if (platform==Windws){shll<-'';            batfile<-'gridCSV.bat'; set<-'set ';}
    
    setenvs<-paste(set,"rngxy='",rngxy,"'\n",
                   set,"xyblksz=-I",delxy,"\n",
                   set,"blocktype=",blktyp,"\n",
                   sep='')
    
    #calculate block means/sums w/ locations at block centers
    script<-paste('blockmean "',xyzFile,'" ${rngxy} ${xyblksz} -C ${blocktype} > tmp.xyg', sep='');
    #shift cell locations so they correspond to pixel centers
    script<-paste(script,paste("gawk '{print $1+",delx/2,", $2+",dely/2,", $3}' tmp.xyg > ",tmpGrid,sep=''),sep='\n')
    
    cat(shll,'\n',
        setenvs,'\n',
        script,' \n',
        file=batfile,sep='');
    
#    if (platform==MacOSX) Sys.chmod(batfile);#make it executable
    
    if (platform==MacOSX) system(paste('/bin/bash ',batfile,sep=''));
    if (platform==Windws) system(batfile,show.output.on.console=TRUE);
    
    dfr2<-read.table(tmpGrid,header=FALSE,sep="",col.names=c("lon","lat",col))
    
    if (cleanup){
        file.remove(batfile);
        file.remove(xyzFile);
        file.remove(tmpGrid);
        file.remove(tmp.xyg);
    }
    
    return(dfr2)
}

#dfr1<-gridCSV(NULL,col='legalret',blocktype='SUM')