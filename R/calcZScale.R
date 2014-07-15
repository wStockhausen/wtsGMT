#'
#'@title Calculate a z scale for plotting data on a map.
#'
#'@description Function to calculate a z scale for plotting data on a map.
#'
#'@param z - data vector to calculate scale from
#'@param logtr - flag (T/F) to ln-transform z before calculating the scale
#'@param satfac - saturation factor relative to maximum
#'
#'@return the scale to be used for plotting z
#'
#'@export
#'      
calcZScale<-function(z,logtr=FALSE,satfac=1.1){
      if (logtr) {
        zscl<-satfac*max(log(z+1)/log(10),na.rm=TRUE);
      } else {
        zscl<-satfac*max(z,na.rm=TRUE);
      }
      return(zscl)
}
