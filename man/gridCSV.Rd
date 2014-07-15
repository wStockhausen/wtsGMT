\name{gridCSV}
\alias{gridCSV}
\title{Grid a dataframe (or csv file) using GMT.}
\usage{
gridCSV(dfr = NULL, lat = "latitude", lon = "longitude", col = NULL,
  logtr = FALSE, xyrng = "180/205/54/62", delx = 0.5, dely = 0.25,
  blocktype = c("MEAN", "SUM"), xyzFile = file.path(getwd(), "tmpXYZ.txt"),
  tmpGrid = file.path(getwd(), "tmpGrid.xyg"), cleanup = FALSE)
}
\arguments{
  \item{dfr}{- the dataframe to grid}

  \item{lat}{- column name containing latitude coordinates}

  \item{lon}{- column name containing longitude
  coordinates}

  \item{col}{- column with value to grid}

  \item{logtr}{- flag (T/F) to log10-transform z values
  before gridding}

  \item{xyrng}{- GMT range string for grid limits, e.g.
  'minLon/maxLon/minLat/maxLat'}

  \item{delx}{- grid cell size in x (longitude)}

  \item{dely}{- grid cell size in y (latitude)}

  \item{blocktype}{- blocking type for values falling
  inside a grid cell ('SUM' or 'MEAN')}

  \item{xyzFile}{- temporary file name for xyz values}

  \item{tmpGrid}{- temporary file name for grid values}

  \item{cleanup}{- flag (T/F) to remove temporary files}
}
\value{
dataframe with the gridded values as column names
'lon','lat', and col.
}
\description{
Function to grid a dataframe or csv file using GMT.
}
\details{

}

