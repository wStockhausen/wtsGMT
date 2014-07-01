\name{gridCSV}
\alias{gridCSV}
\title{Grid a dataframe (or csv file) using GMT.}
\usage{
gridCSV(dfr = NULL, csvin = NULL, lat = "latitude", lon = "longitude",
  col = "legalret", xyrng = "180/205/54/62", delx = 0.4, dely = 0.2,
  blocktype = "SUM", tmpFile = file.path(getwd(), "tmpFile.csv"),
  tmpData = file.path(getwd(), "tmpData.txt"), tmpGrid = file.path(getwd(),
  "tmpGrid.xyg"))
}
\arguments{
  \item{dfr}{- the dataframe to grid}

  \item{csvin}{- the path to the csv file to grid}

  \item{lat}{- column name containing latitude coordinates}

  \item{lon}{- column name containing longitude
  coordinates}

  \item{col}{- column with value to grid}

  \item{xyrng}{- GMT range string for grid limits, e.g.
  'minLon/maxLon/minLat/maxLat'}

  \item{delx}{- grid cell size in x (longitude)}

  \item{dely}{- grid cell size in y (latitude)}

  \item{blocktype}{- blocking type for values falling
  inside a grid cell ('SUM' or 'MEAN')}

  \item{tmpFile}{- temporary csv file name}

  \item{tmpData}{- temporary data file name}

  \item{tmpGrid}{- temporary grid file name}
}
\value{
dataframe with the gridded values
}
\description{
Function to grid a dataframe or csv file using GMT.
}
\details{

}

