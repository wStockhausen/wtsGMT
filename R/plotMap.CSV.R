#'
#'@title Plots a dataframe or csv file on a map using GMT.
#'
#'@description A function to plot a dataframe or csv file on a map using GMT.
#'
#'@details 
#' External Requirements:
#'   * GMT 4.5.x
#'   * on Windows, need both gswin32c and gswin64c installed
#'   * on network, need to use mapped drives to specify files
#'
#' @param   dfr       = dataframe or csv file to plot
#' @param   lat = column name containing latitudes
#' @param   lon = column name containing longitudes
#' @param   col = name of column containing z data
#' @param   gmt = GMT version (4 or 5)
#' @param   title = map title
#' @param   year = year label
#' @param   xyrng  = x-y range for map as GMT string ('xmin/xmax/ymin/ymax')
#' @param   zscl   = z-scale (max) for map
#' @param   zlab   = label for z axes
#' @param   zunits = units for z axes
#' @param   rotate = flag (T/F) or value of angle for map rotation (angle=180 if rotate=T/F)
#' @param   elev   = elevation for map perspective is rotate is not FALSE
#' @param   delx   = x increment for associated grids
#' @param   dely   = y increment for associated grids
#' @param   logtr  = flag to ln-transform z data
#' @param   blocktype          = flag ('MEAN' or 'SUM') for grouping data 
#' @param   plt_blocktype      = flag ('SMOOTH','COARSE') for displaying surface
#' @param   plt_blocklocations = flag to plot block locations as X's
#' @param   plt_surface        = flag to plot data as a color density image
#' @param   plt_bars           = flag to plot data as bars
#' @param   plt_colorscale     = flag to plot color scale
#' @param   plt_reflines       = flag to include refernce lines on map
#' @param   reflines           = list of lists(lon=,lat=) of reference lines to plot
#' @param   plt_title          = flag to include title on map
#' @param   psFile = filename for output file (no extension--will be pdf)
#' @param   pdfDir = directory for output file
#' @param   bathymetryfile = filename of bathymetry to plot
#' @param   cleanup = flag to remove temporary files
#'
#' @return z-scale used for plot.
#' 
#'@importFrom wtsUtilities computeTickInterval
#'@importFrom wtsUtilities getOperatingSystem
#'@importFrom wtsUtilities getCSV
#' 
#' @export
#'
plotMap.CSV<-function(dfr=NULL,
                      lat='latitude',
                      lon='longitude',
                      col=NULL,
                      gmt=4,
                      title='Tanner Crab',
                      year='',
                      xyrng='180/205/54/62',
                      zscl=NULL,
                      zlab='Catch',
                      zunits='num. crab',
                      rotate=FALSE,
                      elev=70,
                      delx=0.5,
                      dely=0.25,
                      blocktype=c('MEAN','SUM'),
                      plt_blocktype=c('SMOOTH','COARSE'),
                      logtr=FALSE,
                      plt_title=FALSE,
                      plt_bars=FALSE,
                      plt_surface=FALSE,
                      plt_blocklocations=FALSE,
                      plt_colorscale=plt_surface|plt_bars,
                      plt_stations=FALSE,
                      plt_reflines=FALSE,
                      reflines=list(list(lon=-166+0*seq(from=50,to=80,by=1),lat=seq(from=50,to=80,by=1))),
                      psFile='catchMaps',
                      toPDF=FALSE,
                      bathymetryFile=file.path(getwd(),'data/depthcontour_200500.prn'),
                      cleanup=FALSE
                      ) {
    
    #check the operating platform
    MacOSX<-'MacOSX';
    Windws<-'Windows';
    platform<-getOperatingSystem();
    
    #check for unc paths
    if (platform==Windws){
        if (length(grep('\\\\',getwd()))>0){
            cat("Working dir is using a network path:\n",
                getwd(),'\n',
                'This function uses Windows .bat files.\n',
                'Please use a mapped drive path for the working directory!!\n',
                'Exiting function\n');
            return;
        }
    }
  
    #read in dataframe, if necessary
    retDFR<-FALSE;
    if (!is.data.frame(dfr)){
        #read in table from csv file
      if (is.null(dfr)) {
        dfr = wtsUtilities::getCSV(caption="Select csv file to plot");
        if (is.null(dfr)) return(NULL);
      } else {
        dfr<-read.csv(dfr,stringsAsFactors=FALSE);
      }
      retDFR<-TRUE;
      names(dfr)<-tolower(names(dfr));
    }
    
    #extract relevant columns, convert lon's to 0-360
    dfr1<-data.frame(lon=dfr[[lon]],lat=dfr[[lat]]);
    dfr1[["lon"]]<-dfr1[["lon"]]*(dfr1[["lon"]]>0) + (360+dfr1[["lon"]])*(dfr1[["lon"]]<0);
    dfr1[["z1"]] <-dfr[[col]];
    
    if (plt_surface){
        #create grid
        dfr1p<-gridCSV(dfr1,lat='lat',lon='lon',col='z1',logtr=logtr,
                       xyrng=xyrng,delx=delx,dely=dely,
                       blocktype=blocktype);
        if (logtr) dfr1p$z1<-(10^dfr1p$z1)-1;#transform back
    }
    
    #calculate z-scaling information    
    if (is.null(zscl)) {
        vls<-dfr1$z1;
        if (plt_surface) vls<-c(vls,dfr1p$z1);
        zscl<-calcZScale(vls,logtr=logtr);#z-scale in (possibly transformed) data units 
    }    
    if (logtr){
        zscale<-zscl;
        dfr1[["z1"]]<-log(dfr1$z1+1)/log(10);#z log10 transformed (not ln)
        zunits<-paste('log@-10@-(',zunits,'+1)',sep='');
    } else {
        z10<-floor(log(zscl)/log(10)); #z-scale normalization factor for factor of 10 scaling
        zscale<-zscl/(10^z10);         #z-scale normalized by factor for factor of 10 scaling
        dfr1[["z1"]]<-dfr1$z1/(10^z10);#z normalized by factor for factor of 10 scaling
        zunits<-paste('10@+',z10,'@+ ',zunits,sep='');
    }
    dfr1[["z2"]]<-dfr1[["z1"]];
    
    #z axis information
    zinc  <-zscale/100;                                     #increment for color scale
    zstride1<-wtsUtilities::computeTickInterval(zscale,1);  #major tick interval
    zstride2<-zstride1/5;                                   #minor tick interval
    
    xyzfile<-file.path(getwd(),'tmp_xyzvals.txt');
    write.table(dfr1,
                xyzfile,
                sep='  ',
                na="",
                quote=FALSE,
                col.names=FALSE,
                row.names=FALSE)
    
    if (plt_reflines){
      str<-'';
      for (refline in reflines){
        str<-paste(str,">\n",sep='');
        lons<-refline$lon*(refline$lon>=0) + (360+refline$lon)*(refline$lon<0);
        lons<-refline$lon;
        lats<-refline$lat;
        for (i in 1:length(lons)){
          str<-paste(str,' ',lons[i],'    ',lats[i],'     0.0 ',"\n",sep='');
        }
      }
      cat(str,file=file.path(getwd(),'reflines.txt'));
    }
    
    yearlabel<-paste(year,sep='');

    psf<-paste(psFile,'.ps',sep='');
    if (file.exists(psf)){
      cat("Deleting old version of '",psf,"'.\n",sep='')
      file.remove(psf);
    }

    blktyp<-'';
    if (blocktype[1]=='SUM') blktyp='-Sz';
    
    if (platform==MacOSX){shll<-'#!/bin/bash +'; batfile<-'plotMap.sh';  set<-'export ';}
    if (platform==Windws){shll<-'';              batfile<-'plotMap.bat'; set<-'set ';}
    
    setenvs<-paste("#--ENVIRONMENT VARIABLES\n",
                   set, 'delx=',delx,'\n',
                   set, 'dely=',dely,'\n',
                   set, 'zlab="',zlab,'"\n',
                   set, 'zscale=',zscale,'\n',
                   set, 'zinc=',zinc,'\n',
                   set, 'zstride1=',zstride1,'\n',
                   set, 'zstride2=',zstride2,'\n',
                   set, 'zunits="',zunits,'"\n',
                   set, 'title="',title,'"\n',
                   set, 'yearlabel="',yearlabel,'"',
                   sep='');
    
    rngxy<-paste("-R",xyrng,sep='');
    rngxyz<-rngxy;
    ymx<-3.65;
    axs<-"WESn";        
    JZ<-'';
    rot3d<-'';
    if (is.logical(rotate)&&(!rotate)) {
        plt_bars<-FALSE;#can't plot bars because elevation is effectively 90
    } else {
        if (is.logical(rotate)) {rotate<-180;} #rotate was TRUE, so set to 180
        rngxyz<-paste(rngxy,'/0/',zscale,sep='');
        ymx<-3.65;
        axs<-"wESn";
        if (gmt==4){
            rot3d<-paste('-E',rotate,'/',elev,sep='');
        } else {
            JZ<-paste('-JZ',ymx,sep='');
            rot3d<-paste('-p',rotate,'/',elev,'/0',sep='');
        }
    }
    cat("rotate =",rotate,"\n");
    cat("rot3d =",rot3d,'\n');
    
    setenvs<-paste(setenvs,"\n",
                   set,"rngxy='",rngxy,"'\n",
                   set,"rngxyz='",rngxyz,"'\n",
                   set,"rot3d='",rot3d,"'\n",
                   set,"JZ='",JZ,"'\n",
                   set,"ymx=",ymx,"\n",
                   set,"axs=",axs,
                   sep='')
    
    delxy<-paste(delx,"/",dely,sep='');
    setenvs<-paste(setenvs,"\n",
                    set,"delxy=",delxy,"\n",
                    set,"mapbndry=-Bpa5f1g1.0/a5f1g1.0",axs,"\n",
                    set,"geotransform=-JB192.5/58/50/65/5.5i\n",
                    set,'yearlabelinfo="181 55.5 16 0 4 BL ',yearlabel,'"\n',
                    set,"mapscl=-L182/55/55/100k\n",
                    set,"xyblksz=-I",delxy,"\n",
                    set,"blocktype=",blktyp,"\n",
                    set,"xs=1i\n",
                    set,"ys=1i",
                   sep='')
    
    #set values for input file and output postscript file
    setenvs<-paste(setenvs,"\n","#--IMPORTANT FILES",sep='\n')    
    setenvs<-paste(setenvs,"\n",set,"infile='",xyzfile,"'\n",sep='')
    setenvs<-paste(setenvs,"\n",set,"postfile='",psf,"'\n",sep='')
    setenvs<-paste(setenvs,"\n",set,"bathymetryfile='",bathymetryFile,"'\n",sep='')
    
    cat("setenvs = ",setenvs,sep='\n')
    
    if (gmt==4) {
        script<-createPlotScript.GMT4(delx=delx,
                                      dely=dely,
                                      logtr=logtr,
                                      blocktype=blocktype[1],
                                      plt_blocktype=plt_blocktype[1],
                                      plt_surface=plt_surface,
                                      plt_bars=plt_bars,
                                      plt_blocklocations=plt_blocklocations,
                                      plt_stations=plt_stations,
                                      plt_colorscale=plt_colorscale,
                                      plt_reflines=plt_reflines,
                                      plt_title=plt_title,
                                      cleanup=cleanup);
    }
    cat("script = ",script,sep='\n');
    
    cat(shll,'\n',
        setenvs,'\n',
        script,
        file=batfile,sep='');
#    readline(prompt='start runGMT >');
    
#    if (platform==MacOSX) Sys.chmod(batfile);#make it executable
    
    cat("Starting GMT\n")
    if (platform==MacOSX) system(paste('/bin/bash ',batfile,sep=''));
    if (platform==Windws) system(batfile,show.output.on.console=TRUE);

    if (cleanup) {
        file.remove(xyzfile);
        file.remove(batfile);
    }
    
    cat("Finished running GMT portion\n");
    
    cat('\nzscale used for plots was ',zscl,'\n\n\n');
    if (retDFR) return(invisible(list(dfr=dfr,zscl=zscl)));
    return(invisible(zscl));
}

