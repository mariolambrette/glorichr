#' Import hydrochemistry and sampling location data
#'
#' This function imports the hydrochemstry.csv file from the GLORICH
#' database, and has the option to add location data from 'sampling_locations.csv'
#' to each sample.
#'
#' The data columns from sampling_locations.csv that are added are:
#''Country', 'State', 'Latitude', 'Longitude', 'CoordinateSystem'
#' .
#' @param hydro_filepath The file path to the hydrochemsitry.csv file (either an absolute path, or a relative path from the current wd)
#' @param location A boolean determining whether location data should be included, false by default
#' @param loc_filepath The file path to the sampling_location.csv file (either an absolute path, or a relative path from the current wd)
#' @return A dataframe containing data from hydrochemistry.csv
#' @export
#' @examples
#' LoadHydroData(hydro_filepath = 'hydrochemistry.csv', location = T, loc_filepath = 'sampling_locations.csv')
#' LoadHydroData(hydro_filepath = 'hydrochemistry.csv')


LoadHydroData <- function(hydro_filepath, location = F, loc_filepath){
  hydrochem <- read.csv(hydro_filepath)
  if(location){
    sampling_location <- read.csv(loc_filepath)
    loc <- sampling_location[c('STAT_ID', 'Country', 'State', 'Latitude',
                               'Longitude', 'CoordinateSystem')]
    df <- merge(hydrochem, loc, by = 'STAT_ID')
  }
  else{
    df <- hydrochem
  }

  return(df)
}
