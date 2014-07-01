\name{plotMap.CSV}
\alias{plotMap.CSV}
\title{Plots a dataframe or csv file on a map using GMT.}
\usage{
plotMap.CSV(dfr = NULL, csvin = NULL, lat = "latitude",
  lon = "longitude", col = NULL, gmt = 4, label = "Tanner Crab",
  year = "", xyrng = "180/205/54/62", zscl = NULL, ztype = "Catch",
  zunits = "crab", delx = 0.5, dely = 0.25, blocktype = c("MEAN",
  "SUM"), plt_blocktype = c("SMOOTH", "COARSE"), logtr = FALSE,
  rotate = FALSE, plt_title = FALSE, plt_bars = FALSE,
  plt_surface = FALSE, plt_blocklocations = FALSE,
  plt_colorscale = plt_surface | plt_bars, plt_stations = FALSE,
  plt_reflines = FALSE, reflines = list(list(lon = -166 + 0 * seq(from = 50,
  to = 80, by = 1), lat = seq(from = 50, to = 80, by = 1))), showMap = FALSE,
  psFile = "catchMaps", pdfDir = "", bathymetryFile = file.path(getwd(),
  "data/depthcontour_200500.prn"), debug = FALSE)
}
\arguments{
  \item{dfr}{= dataframe to plot}

  \item{csvin}{= csv file to plot}

  \item{csvout}{= output csv file for CPUE}

  \item{lat}{= column name containing latitudes}

  \item{lon}{= column name containing longitudes}

  \item{col}{= name of column containing z data}

  \item{gmt}{= GMT version (4 or 5)}

  \item{label}{= map title}

  \item{year}{= year label}

  \item{xyrng}{= x-y range for map as GMT string
  ('xmin/xmax/ymin/ymax')}

  \item{psFile}{= filename for output EPS file (no
  extension--will be eps)}

  \item{batchFile}{= path to plotCatch.bat}

  \item{bathymetryFile}{= filename of bathymetry to plot}

  \item{epsToolFile}{= filename of EPS Tool batch file}

  \item{zscl}{= z-scale (max) for map}

  \item{ztype}{= label for z axes}

  \item{zunits}{= units for z axes}

  \item{delx}{= x increment for associated grids}

  \item{dely}{= y increment for associated grids}

  \item{logtr}{= flag to ln-transform z data}

  \item{blocktype}{= flag ('MEAN' or 'SUM') for grouping
  data}

  \item{plt_blocktype}{= flag ('SMOOTH','COARSE') for
  displaying surface}

  \item{plt_surface}{= flag to plot data as a color density
  image}

  \item{plt_blocklocations}{= flag to plot block locations
  as X's}

  \item{plt_bars}{= flag to plot data as bars}

  \item{plt_colorscale}{= flag to plot color scale}

  \item{plt_reflines}{= flag to include refernce lines on
  map}

  \item{reflines}{= list of lists(lon=,lat=) of reference
  lines to plot}

  \item{plt_title}{= flag to include title on map}

  \item{showMap}{= flag (T/F) to view EPS plots using
  GSView}

  \item{psFile}{=}

  \item{pdfDir}{=}

  \item{bathymetryfile}{=}
}
\value{
z-scale used for plot.
}
\description{
A function to plot a dataframe or csv file on a map using
GMT.
}
\details{


External Requirements:\n --GMT 4.5.x --need both gswin32c
and gswin64c installed\n --on network, need to use mapped
drives to specify files
}

