\name{plotMap.CSV}
\alias{plotMap.CSV}
\title{Plots a dataframe or csv file on a map using GMT.}
\usage{
plotMap.CSV(dfr = NULL, lat = "latitude", lon = "longitude", col = NULL,
  gmt = 4, title = "Tanner Crab", year = "", xyrng = "180/205/54/62",
  zscl = NULL, zlab = "Catch", zunits = "num. crab", rotate = FALSE,
  elev = 70, delx = 0.5, dely = 0.25, blocktype = c("MEAN", "SUM"),
  plt_blocktype = c("SMOOTH", "COARSE"), logtr = FALSE, plt_title = FALSE,
  plt_bars = FALSE, plt_surface = FALSE, plt_blocklocations = FALSE,
  plt_colorscale = plt_surface | plt_bars, plt_stations = FALSE,
  plt_reflines = FALSE, reflines = list(list(lon = -166 + 0 * seq(from = 50,
  to = 80, by = 1), lat = seq(from = 50, to = 80, by = 1))),
  psFile = "catchMaps", toPDF = FALSE, bathymetryFile = file.path(getwd(),
  "data/depthcontour_200500.prn"), cleanup = FALSE)
}
\arguments{
  \item{dfr}{= dataframe or csv file to plot}

  \item{lat}{= column name containing latitudes}

  \item{lon}{= column name containing longitudes}

  \item{col}{= name of column containing z data}

  \item{gmt}{= GMT version (4 or 5)}

  \item{title}{= map title}

  \item{year}{= year label}

  \item{xyrng}{= x-y range for map as GMT string
  ('xmin/xmax/ymin/ymax')}

  \item{zscl}{= z-scale (max) for map}

  \item{zlab}{= label for z axes}

  \item{zunits}{= units for z axes}

  \item{rotate}{= flag (T/F) or value of angle for map
  rotation (angle=180 if rotate=T/F)}

  \item{elev}{= elevation for map perspective is rotate is
  not FALSE}

  \item{delx}{= x increment for associated grids}

  \item{dely}{= y increment for associated grids}

  \item{logtr}{= flag to ln-transform z data}

  \item{blocktype}{= flag ('MEAN' or 'SUM') for grouping
  data}

  \item{plt_blocktype}{= flag ('SMOOTH','COARSE') for
  displaying surface}

  \item{plt_blocklocations}{= flag to plot block locations
  as X's}

  \item{plt_surface}{= flag to plot data as a color density
  image}

  \item{plt_bars}{= flag to plot data as bars}

  \item{plt_colorscale}{= flag to plot color scale}

  \item{plt_reflines}{= flag to include refernce lines on
  map}

  \item{reflines}{= list of lists(lon=,lat=) of reference
  lines to plot}

  \item{plt_title}{= flag to include title on map}

  \item{psFile}{= filename for output file (no
  extension--will be pdf)}

  \item{pdfDir}{= directory for output file}

  \item{bathymetryfile}{= filename of bathymetry to plot}

  \item{cleanup}{= flag to remove temporary files}
}
\value{
z-scale used for plot.
}
\description{
A function to plot a dataframe or csv file on a map using
GMT.
}
\details{


External Requirements: * GMT 4.5.x * on Windows, need both
gswin32c and gswin64c installed * on network, need to use
mapped drives to specify files
}

