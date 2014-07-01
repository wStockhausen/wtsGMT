#'@details 
#' External Requirements:\n
#'   --need both gswin32c and gswin64c installed\n
#'   --on network, need to use mapped drives to specify files
#'
#'
#' @param   dfr       = standardized CPUE data table previously extracted
#' @param   csvin     = csv file with catch previously extracted
#' @param   csvout    = output csv file for CPUE
#' @param   psFile    = filename for output EPS file (no extension--will be eps)
#' @param   batchFile      = path to plotCatch.bat
#' @param   bathymetryFile = filename of bathymetry to plot
#' @param   epsToolFile    = filename of EPS Tool batch file
#' @param   plt_title = include species name as title in plot
#' @param   plt_bars  = plot CPUE as bars
#' @param   plt_surf  = plot CPUE as color density
#' @param   plt_clrbr = plot color bar
#' @param   zscl      = z-scale
#' @param   showMap   = flag (T/F) to view EPS plots using GSView
#'
#' @return z-scale used for plot.
#' 
#' @importFrom wtsUtilities getCSV
#' 
#' @export
#'
#source(file.path(Sys.getenv("R_SCRIPTS"),"Utilities/functionUtilities.R"),chdir=TRUE);
plotMap.CSV<-function(dfr=NULL,
                      csvin=NULL,
                      lat='latitude',
                      lon='longitude',
                      col=NULL,
                      gmt=4,
                      label='Tanner Crab',
                      year='',
                      xyrng='180/205/54/62',
                      zscl=NULL,
                      ztype='Catch',
                      zunits='crab',
                      delx=0.5,
                      dely=0.25,
                      blocktype=c('MEAN','SUM'),
                      plt_blocktype=c('SMOOTH','COARSE'),
                      psFile='catchMaps',
                      pdfDir='',
                      pdfFile='catchMaps',
                      logtr=FALSE,
                      rotate=FALSE,
                      plt_title=FALSE,
                      plt_bars=FALSE,
                      plt_surface=FALSE,
                      plt_blocklocations=FALSE,
                      plt_colorscale=plt_surface|plt_bars,
                      plt_stations=FALSE,
                      plt_reflines=FALSE,
                      reflines=list(list(lon=-166+0*seq(from=50,to=80,by=1),lat=seq(from=50,to=80,by=1))),
                      showMap=FALSE,
                      bathymetryFile=file.path(getwd(),'data/depthcontour_200500.prn'),
                      epsToolFile=   file.path(getwd(),'runEPStool'),
                      debug=FALSE
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
  
    if (is.null(dfr)){
        #read in table from csv file
      if (is.null(csvin)) {
        dfr = wtsUtilities::getCSV(caption="Select csv file to plot");
        if (is.null(dfr)) return(NULL);
      } else {
        dfr<-read.csv(csvin,stringsAsFactors=FALSE);
      }
      names(dfr)<-tolower(names(dfr));
    }
    
    #save to temporary file csvfile (must do this even for input csvfile) for plots
    dfr[[lon]]<-dfr[[lon]]*(dfr[[lon]]>0) + (360+dfr[[lon]])*(dfr[[lon]]<0);
    dfr1<-data.frame(longitude=dfr[[lon]],latitude=dfr[[lat]]);
    dfr1[[col]]<-dfr[[col]];
    
    if (is.null(zscl)) {
      if (logtr) {
        zscl<-0.9*max(log(dfr1[,-(1:2)]+1)/log(10),na.rm=TRUE);
      } else {
        zscl<-0.8*max(dfr1[,-(1:2)],na.rm=TRUE);
      }
    }
    
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
    
    z10<-0;
    if (!logtr) z10<-floor(log(zscl)/log(10));
    zscl<-zscl/(10^z10); #normalize to factor of 10 scaling
    zinc<-zscl/100;
    zstride1<-computeTickInterval(zscl,1);
    zstride2<-zstride1/5;
    
    #save to temporary file csvfile 
    csvfile<-file.path(getwd(),'tmp_gmt.csv');
    write.table(dfr1,
                csvfile,
                sep=',',
                na="",
                quote=FALSE,
                col.names=FALSE,
                row.names=FALSE)

    #inputs for the gmt batch files:
    # %1 = input csv filename
    # %2 = output postscript filename
    yearlabel<-paste(year,sep='');
    psfname<-psFile;

    psf<-paste(psfname,'.ps',sep='');
    if (file.exists(psf)){
      cat("Deleting old version of '",psf,"'.\n",sep='')
      file.remove(psf);
    }
    
    epsf<-paste(psfname,'.eps',sep='')
    if (file.exists(epsf)){
      cat("Deleting old version of '",epsf,"'.\n")
      file.remove(epsf);
    }

    blktyp<-'';
    if (blocktype[1]=='SUM') blktyp='-Sz';
    if (!logtr) zunits<-paste('10@+',z10,'@+',zunits,sep='');
    
    if (platform==MacOSX){shll<-'#!/bin/bash +'; batfile<-'plotMap.sh';  set<-'export ';}
    if (platform==Windws){shll<-'';              batfile<-'plotMap.bat'; set<-'set ';}
    
    setenvs<-paste("#--ENVIRONMENT VARIABLES\n",
                   set, 'spplabel="',label,'"\n',
                   set, 'delx=',delx,'\n',
                   set, 'dely=',dely,'\n',
                   set, 'zscale=',zscl,'\n',
                   set, 'zscaleint=',zinc,'\n',
                   set, 'zstride1=',zstride1,'\n',
                   set, 'zstride2=',zstride2,'\n',
                   set, 'z10=',z10,'\n',
                   set, 'ztype=',ztype,'\n',
                   set, 'zunits=',zunits,'\n',
                   set, 'yearlabel=',yearlabel,'\n',
                   sep='');
    
    rngxy<-paste("-R",xyrng,sep='');
    rngxyz<-rngxy;
    ymx<-3.5;
    axs<-"WESn";        
    JZ<-'';
    rot3d<-'';
    if (rotate!=FALSE){
        rngxyz<-paste(rngxy,'/0/',zscl,sep='');
        ymx<-2.3;
        axs<-"wESn";
        if (gmt==4){
            if (rotate==TRUE){rot3d<-'-E180/70';}
            if (rotate==170) {rot3d<-'-E170/70';}
            if (rotate==160) {rot3d<-'-E160/70';}
        } else {
            JZ<-paste('-JZ',ymx,sep='');
            if (rotate==TRUE){rot3d<-'-p180/40/0';}
            if (rotate==170) {rot3d<-'-p170/40/0';}
            if (rotate==160) {rot3d<-'-p160/40/0';}
        }
    }
    setenvs<-paste(setenvs,"\n",
                   set,"rngxy='",rngxy,"'\n",
                   set,"rngxyz='",rngxyz,"'\n",
                   set,"rot3d='",rot3d,"'\n",
                   set,"JZ='",JZ,"'\n",
                   set,"ymx=",ymx,"\n",
                   set,"axs=",axs,sep='')
    
    delxy<-paste(delx,"/",dely,sep='');
    setenvs<-paste(setenvs,"\n",
                    set,"delxy=",delxy,"\n",
                    set,"mapbndry=-Bpa5f1g0.5/a5f1g0.25",axs,"\n",
                    set,"geotransform=-JB192.5/58/50/65/5.5i\n",
                    set,'yearlabelinfo="181 55.5 16 0 4 BL ',yearlabel,'"\n',
                    set,"mapscl=-L182/55/55/100k\n",
                    set,"xyblksz=-I",delxy,"\n",
                    set,"xs=1i\n",
                    set,"ys=1i",sep='')
    
    #set values for input file and output postscript file
    setenvs<-paste(setenvs,"\n","#--IMPORTANT FILES",sep='\n')    
    setenvs<-paste(setenvs,"\n",set,"infile='",csvfile,"'\n",sep='')
    setenvs<-paste(setenvs,"\n",set,"postfile='",psf,"'\n",sep='')
    setenvs<-paste(setenvs,"\n",set,"bathymetryfile='",bathymetryFile,"'\n",sep='')
    
    if (gmt==4) {
        script<-createPlotScript.GMT4(z10=z10,
                                      delx=delx,
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
                                        plt_title=plt_title);
    }
    
    strVw<-'';
    if (showMap) {
        if (platform==Windws) {strVw<-paste('call "%GhostView_HOME%\\gsview64" ',epsf);}
    }
    
    cat(shll,'\n',
        setenvs,'\n',
        'echo $PATH','\n',
        script,
        file=batfile,sep='');
#    readline(prompt='start runGMT >');
    
    if (platform==MacOSX) Sys.chmod(batfile);#make it executable
    
    cat("Starting GMT\n")
    if (platform==MacOSX) system(paste('/bin/bash ',batfile,sep=''));
    if (platform==Windws) system(batfile,show.output.on.console=TRUE);
#    file.remove(batfile)
    
    cat("Finished running GMT portion\n");
    
#    file.remove(csvfile);
#    file.remove(psf);
    
#     if (!is.null(pdfFile)) {
#       readline(prompt="Creating pdf file. Hit return: \n")    
#       createPDFfromEPS(pdfFile,epsf);
#     }
    
    cat('\nzscale used for plots was ',zscl,'\n\n\n');
    return(zscl);
}

