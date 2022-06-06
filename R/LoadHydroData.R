#' Import hydrochemistry and sampling location data
#'
#' This function imports the hydrochemstry.csv file from the GLORICH
#' database, and has the option to add location data from 'sampling_locations.csv'
#' to each sample.
#'
#' The data columns from sampling_locations.csv that are added are:
#''Country', 'State', 'Latitude', 'Longitude', 'CoordinateSystem'
#' .
#' @param location A boolean determining whether location data should be included
#' @return A dataframe containing data from hydrochemistry.csv
#' @export
#' @examples
#' LoadHydroData(location = F)


LoadHydroData <- function(location){
  hydrochem <- read.csv('hydrochemistry.csv')
  if(location){
    sampling_location <- read.csv('sampling_locations.csv')
    loc <- sampling_location[c('STAT_ID', 'Country', 'State', 'Latitude',
                                    'Longitude', 'CoordinateSystem')]
    df <- merge(hydrochem, loc, by = 'STAT_ID')
  }
  else{
    df <- hydrochem
  }

  return(df)
}
