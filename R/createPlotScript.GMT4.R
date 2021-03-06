#'
#'@title Creates a shell script for GMT 4.5 to plot a dataframe or csv file on a map.
#'
#'@description A function to create a shell script for GMT 4.5 to plot a dataframe or csv file on a map.
#'
#'@details 
#' External Requirements:
#'   * GMT 4.5.x
#'
#' @param   delx = x increment for associated grids
#' @param   dely = y increment for associated grids
#' @param   logtr = flag to ln-transform z data
#' @param   blocktype = flag ('MEAN' or 'SUM') for grouping data 
#' @param   plt_blocktype = flag ('SMOOTH','COARSE') for displaying surface
#' @param   plt_surface = flag to plot data as a color density image
#' @param   plt_blocklocations = flag to plot block locations as X's
#' @param   plt_bars = flag to plot data as bars
#' @param   plt_colorscale = flag to plot color scale
#' @param   plt_reflines = flag to include refernce lines on map
#' @param   plt_title = flag to include title on map
#' @param   cleanup = flag to remove temporary files
#'
#' @return GMT function script as character vector
#' 
#' @importFrom wtsUtilities getCSV
#' 
#' @export
#'
createPlotScript.GMT4<-function(delx=1,
                                dely=1,
                                logtr=FALSE,
                                blocktype=c('MEAN','SUM'),
                                plt_blocktype=c('SMOOTH','COARSE'),
                                plt_surface=FALSE,
                                plt_bars=FALSE,
                                plt_blocklocations=FALSE,
                                plt_stations=FALSE,
                                plt_colorscale=FALSE,
                                plt_reflines=FALSE,
                                plt_title=FALSE,
                                cleanup=TRUE){
    script<-'';
    #
    # GMT script to plot catch or CPUE for CRAB
    #  The following environment variables should be exported prior to
    #  calling this batch file.
    #      postfile       = output postscript file
    #      zscale         = scale for z axes
    #      zinc           = increment for z color zscale
    #      zstride1       = major tick interval for z scale
    #      zstride2       = minor tick interval for z scale
    #      blocktype      = flag indicating type of blockmean processing: MEAN (default: -S) or SUM (-Sz)
    #      plt_bars       = plot z-values as bars (TRUE/FALSE)
    #      plt_surface    = plot z-values as color density (TRUE/FALSE)
    #      plt_stations   = include stations on plot (TRUE/FALSE)
    #      plt_title      = include title (TRUE/FALSE)
    #      ztype          = type of data for z axis
    #      zunits         = units for z axis
    #      bathymetryfile = full path to bathymetry file (paths w/ spaces must be quoted)
     
    #  The input xyz file should have the following structure
    #      col 1: longitude
    #      col 2: latitude
    #      col 3: z
    #      col 4: z 
    
        
    # export  plotting defaults
    #      export  paper media for EPS file (appended "+")
    script<-paste(script,"\n","#--PLOT FORMAT DEFAULTS",sep='\n')
    script<-paste(script,"gmtset PAPER_MEDIA Custom_612x400",sep='\n')
    script<-paste(script,"gmtset COLOR_FOREGROUND 255/0/0",sep='\n')
    script<-paste(script,"gmtset BASEMAP_TYPE fancy PAGE_ORIENTATION portrait X_ORIGIN 1 X_ORIGIN 1",sep='\n')
    script<-paste(script,"gmtset ANNOT_FONT_SIZE 12p ANNOT_FONT_SIZE_SECONDARY 14p HEADER_FONT_SIZE 24p LABEL_FONT_SIZE 14p",sep='\n')
    script<-paste(script,"gmtset FRAME_WIDTH 0.075 PLOT_DEGREE_FORMAT DF OUTPUT_DEGREE_FORMAT D",sep='\n')
    
    #NOTE: '> file' writes standard output to 'file'
    #NOTE: '>> file' appends standard output to 'file'
    
    # plot basemap and title
    script<-paste(script,"\n","#--PLOT BASE MAP",sep='\n')
    script<-paste(script,"psbasemap ${rngxyz} ${geotransform} ${rot3d} -X${xs} -Y${ys} ${mapscl} ${mapbndry} -K > ${postfile}",sep='\n')
    
    #expand page size by adding 'invisible' characters to lower left, upper right corners
    rmx<-6.1;
    if (plt_colorscale) rmx<-7.0;
    script<-paste(script,"\n","#--EXPAND PAGE SIZE",sep='\n');
    script<-paste(script,"\n",paste("rmx=",rmx,'\n',sep=''));
    script<-paste(script,"echo  -0.5   -0.45  24 0 4 LB L|pstext -R0/1/0/1/ -JX1i -G254   -N -O -K >> ${postfile}",sep='\n')
    script<-paste(script,"echo  ${rmx} ${ymx} 24 0 4 TR R|pstext -R0/1/0/1/ -JX1i -G254   -N -O -K >> ${postfile}",sep='\n')
    
    # Make a color palette table
    script<-paste(script,"\n","#--MAKE COLOR PALETTE",sep='\n')
    script<-paste(script,"makecpt -Cseis -T0/${zscale}/${zinc} -I -Z -D > clrs.cpt",sep='\n')
        
    # Create gridded surface
    if (plt_surface) {
        script<-paste(script,"\n","#--GRID AND PLOT SURFACE",sep='\n')
        # Calculate block means, with output locations at cell center [-C]
        script<-paste(script,"blockmean ${infile}  ${rngxy} ${xyblksz} -C ${blocktype} > tmp.xyg",sep='\n')
        if (plt_blocktype[1]=='SMOOTH') {
            #create smooth surface
            script<-paste(script,"surface tmp.xyg -Gtmp.grd ${rngxy} -I0.1/0.1 -Ll0 -S2 -T0.5",sep='\n');
            script<-paste(script,"grdview tmp.grd ${rngxy} ${geotransform} ${rot3d} -Cclrs.cpt -S -Qi -O -K >> ${postfile}",sep='\n');
            #clip the surface
            script<-paste(script,"psmask tmp.xyg ${rngxyz} ${geotransform} ${rot3d} -G255 -I1 -N -V -O -K >> ${postfile}",sep='\n');
            #turn clipping off
            script<-paste(script,"psmask -C -O -K >> ${postfile}",sep='\n');
            #replot the basemap (w/out X,Y shift), as this may have been clipped
            script<-paste(script,"psbasemap ${rngxyz} ${geotransform} ${rot3d} ${mapscl} ${mapbndry} -O -K >> ${postfile}",sep='\n')
        } else if (plt_blocktype[1]=='COARSE') {
            #shift cell locations to pixel centers
            script<-paste(script,paste("gawk '{print $1+",delx/2,", $2+",dely/2,", $3}' tmp.xyg > tmp1.xyg",sep=''),sep='\n')
            #create coarse grid using pixel registration [-F]
            script<-paste(script,"xyz2grd tmp1.xyg -Gtmp.grd ${rngxy} ${xyblksz} -F ",sep='\n');
            # plot contour surface (use -Tso instead of -Ts to outline the tiles, as well color them
            script<-paste(script,"grdview tmp.grd ${rngxy} ${geotransform} ${rot3d} -Cclrs.cpt -Tsored -O -K >> ${postfile}",sep='\n');
        }
    }

    # plot the coast
    script<-paste(script,"\n","#--PLOT COASTLINE",sep='\n')
    script<-paste(script,"pscoast ${rngxyz} ${geotransform} ${rot3d} ${mapscl} -G0/200/0 -Dh -W -O -K >> ${postfile}",sep='\n')
    
    # depth contour...
    script<-paste(script,"\n","#--PLOT BATHYMETRY",sep='\n')
    script<-paste(script,"psxyz ${bathymetryfile} ${rngxyz} ${geotransform} ${rot3d} -M -W1,150 -O -K >> ${postfile}",sep='\n')
    
    if (plt_reflines){ 
        script<-paste(script,"\n","#--PLOT REFERENCE LINES",sep='\n')
        script<-paste(script,"psxyz reflines.txt ${rngxyz} ${geotransform} ${rot3d} -M -W4,50,-. -O -K >> ${postfile}",sep='\n')
    }
    
    # plot actual station locations...
    #[notes: delimiter in ${infile} is a comma, so need to specify '-F,'. ',' in print stmt substituted w/ the delimiter] 
    if (plt_stations){
        script<-paste(script,"\n","#--PLOT STATIONS",sep='\n')
        script<-paste(script,"gawk '{print $1, $2, 0.0}' ${infile} > tmplocs.txt",sep='\n')
        script<-paste(script,"psxyz tmplocs.txt ${rngxyz} ${geotransform} ${rot3d} -Ss0.01i -W2,red -O -K >> ${postfile}",sep='\n')
    }
    
    #plot block locations 
    #[notes: delimiter in tmp.xyg is whitespace, the default delimiter, so need for -F. ',' in print stmt substituted w/ the delimiter] 
    if (plt_blocklocations){
        script<-paste(script,"\n","#--PLOT BLOCK LOCATIONS",sep='\n')
        if (plt_blocktype[1]=='SMOOTH') {
            script<-paste(script,"gawk '{print $1, $2, 0.0}' tmp.xyg > tmplocs1.txt",sep='\n')
        } else if (plt_blocktype[1]=='COARSE') {
            script<-paste(script,"gawk '{print $1, $2, 0.0}' tmp1.xyg > tmplocs1.txt",sep='\n')
        }
        script<-paste(script,'psxyz tmplocs1.txt ${rngxyz} ${geotransform} ${rot3d} -Sx0.1i -W2,blue -O -K >> ${postfile}',sep='\n')
    }
    
    # Plot values as columns above locations
    if (plt_bars){
        script<-paste(script,"\n","#--PLOT BARS",sep='\n')
        if (plt_blocktype[1]=='SMOOTH'){
            script<-paste(script,'psxyz "${infile}" ${rngxyz} ${geotransform} ${rot3d} -B//a${zstride1}f${zstride2}:"${zlab} (${zunits})":Z -JZ1.5i -Cclrs.cpt -So0.05i    -W -K -O >> ${postfile}',sep='\n')
        } else if (plt_blocktype[1]=='COARSE'){
            script<-paste(script,paste("gawk '{print $1, $2, $3, $3,",delx,",",delx,"}' tmp.xyg > tmp1.xyg",sep=''),sep='\n')
            script<-paste(script,'psxyz tmp1.xyg ${rngxyz} ${geotransform} ${rot3d} -B//a${zstride1}f${zstride2}:"${zlab} (${zunits})":Z -JZ1.5i -Cclrs.cpt -Sou -W -K -O >> ${postfile}',sep='\n')
        }
    }
    
    # Label the year for each panel
    script<-paste(script,"\n","#--PLOT YEAR LABEL",sep='\n')
    script<-paste(script,'echo ${yearlabelinfo}|pstext  ${rngxyz} ${geotransform} ${rot3d} -G0 -N -O -K >> ${postfile}',sep='\n')
    
    # Plot the cpue zscale
    if (plt_colorscale){
        script<-paste(script,"\n","#--PLOT COLOR SCALE",sep='\n')
        script<-paste(script,'export  clrbarloc=-D6.2i/1i/1.5i/0.25i',sep='\n')
        script<-paste(script,'psscale ${clrbarloc} -Cclrs.cpt -Ba${zstride1}f${zstride2}:" ":/:"${zunits}": -N -O -K >> ${postfile}',sep='\n')
        script<-paste(script,'echo 0 0 14 90 4 MC ${zlab}|pstext -R0/1/0/1 -JX1i -G0 -N -Xa6.8i -Ya1i -O -K >> ${postfile}',sep='\n')
    }
    
    #Add title and close ps file
    if (plt_title){ 
        script<-paste(script,"\n","#--ADD TITLE & CLOSE FILE",sep='\n')
        script<-paste(script,'echo  6.0  ${ymx}  20 0 4 MR "${title}"|pstext -R0/1/0/1/ -JX1i -G0   -N -O >> ${postfile}',sep='\n')
    } else {
        script<-paste(script,"\n","#--ADD NO TITLE & CLOSE FILE",sep='\n')
        script<-paste(script,'echo  6.0  ${ymx}  20 0 4 MR o|pstext          -R0/1/0/1/ -JX1i -G255 -N -O >> ${postfile}',sep='\n')
    }
    
#     #convert ps file to PNG and PDF formats [-Tg, -Tf]
#     script<-paste(script,"\n","#--convert ps file to PNG and PDF formats",sep='\n')
# #    script<-paste(script,"ps2raster ${postfile} -A -S -Tg",sep='\n')
#     script<-paste(script,"ps2raster ${postfile} -A -S -Tf",sep='\n')

    #clean up temporary files
    if (cleanup){
        script<-paste(script,"\n","#--CLEAN UP",sep='\n')
        script<-paste(script,'rm -f tmp.xyg',sep='\n')
        script<-paste(script,'rm -f tmp1.xyg',sep='\n')
        script<-paste(script,'rm -f tmp.grd',sep='\n')
        script<-paste(script,'rm -f tmplocs.txt',sep='\n')
        script<-paste(script,'rm -f tmplocs1.txt',sep='\n')
        script<-paste(script,'rm -f reflines.txt',sep='\n')
#        script<-paste(script,"rm -f ${postfile}",sep='\n')
        script<-paste(script,'rm -f clrs.cpt',sep='\n')
        script<-paste(script,'rm -f .gmtcommands4',sep='\n')
        script<-paste(script,'rm -f .gmtdefaults4',sep='\n')
    }

    return(script);
}