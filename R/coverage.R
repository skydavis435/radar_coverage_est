coverage <- function()
{
  #Load packages
  require(geosphere)
  require(sp)
  #maybe stuff for maps later?
  #Define variables.  Maybe make this a user input thing later.  
  #Currently set for a non-rotating BSTAR-X as the SW radar at DFW, covering from 0 to 70 degrees above horizon, looking due North
  #radar location in UTM/meters ASL
  radar.northing <- 3639196.09
  radar.easting <- 682072.14
  location <- c(radar.northing,radar.easting)
  names(location) <- c("Northing", "Easting")
  radar.elevation <- 175
  radar.elevation.beam.deg <- 70
  radar.elevation.boresight.deg <- 35
  radar.elevation.resolution.bins <- 8
  radar.azimuth.beam.deg <- 90
  radar.azimuth.resolution.bins <- 32
  radar.azimuth.boresight.deg <- 0
  #convert everything to radians
  radar.elevation.beam <-pi*radar.elevation.beam.deg/180
  radar.elevation.boresight <- pi*radar.elevation.boresight.deg/180
  radar.elevation.resolution <- radar.elevation.beam/radar.elevation.resolution.bins
  radar.azimuth.beam <- pi*radar.azimuth.beam.deg/180
  radar.azimuth.boresight <- pi*radar.azimuth.boresight.deg/180
  radar.azimuth.resolution <- radar.azimuth.beam/radar.azimuth.resolution.bins

  #calculate things for beams
  radar.elevation.beam.top <-radar.elevation.boresight+radar.elevation.beam/2
  radar.elevation.beam.bottom <- -1*radar.elevation.boresight+radar.elevation.beam/2
  radar.azimuth.beam.left <- radar.azimuth.boresight - radar.azimuth.beam/2
  radar.azimuth.beam.right <- radar.azimuth.boresight + radar.azimuth.beam/2

  #make sure I got the math right.  Display everything so far
  cat("radar.elevation.beam.top: ",radar.elevation.beam.top*180/pi, "degrees")
  cat("\n")
  cat("radar.elevation.beam.bottom: ",radar.elevation.beam.bottom*180/pi, "degrees")
  cat("\n")
  cat("radar.azimuth.beam.left: ",radar.azimuth.beam.left*180/pi, "degrees")
  cat("\n")
  cat("radar.azimuth.beam.right: ",radar.azimuth.beam.right*180/pi, "degrees")
  cat("\n")
  cat("Location: ",location)
  
}
