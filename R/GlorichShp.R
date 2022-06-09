#' Create a shapefile with all sampling locations
#'
#' This function provides a number of options for generating shapefiles from the
#' GLORICH database. Individual shapefiles for each corrdinate system can be
#' created - or all coordinate systems can be converted to one specified system
#' and merged into a large shapefile containing all input points
#'
#' .
#' @param data The complete hydrochemistry dataset as produced by LoadHydroData
#'  when 'location' is specified as true. Any subset of this dataset can also be
#'  used as an input here - though the parameter 'coord_sys' should be updated
#'  accordingly
#' @param coord_sys The is a list of vectors of length 2. Each vector in the list
#'   contains: (1) The name of the coordinate reference system as it appears in
#'   the 'CoordinateSystem' Column of the database and (2) the EPSG code for
#'   said coordinate system. The default contains every unique system label in the
#'   database as of 08/06/2022. Users are encouraged to check whether these are
#'   correct before using the function
#' @param convert A logical vector indicating whether the user would like every
#'   point to be converted to the same coordinate reference system
#' @param final_CRS The EPSG code for the coordinate reference system to which each
#'   point should be converted. Must be enetered as a character string containing
#'   only the digits of the code. Default is '4362' - the code for WGS84.
#' @param export A logical vector indicating whether the user would like the
#'   created SpatialPointsDataFrames to be exported as .shp files. If TRUE (default)
#'   either a complete shapefile, containing all data points in the specified
#'   coordinate reference system or multiple shapefiles for each different system
#'   will be exported depnding on whether 'convert' is TRUE or FALSE
#' @param filename A character string contaning the desired name of the file to export.
#'   Only required if 'export' is TRUE. If 'convert' is TRUE a single character string
#'   is required. If 'convert' is FALSE a list of filenames of the same length as the
#'   list passed to 'coord_sys' is required
#' @param filepath The absolute filepath for the export location of the shapefiles.
#'   Only required if 'export' is TRUE. Defaults to the current working directory.
#' @return Either one large SpatialPointsDataFrame in the specified coordinate
#'   reference system or a list of SPDFs in the original coordinate reference systems
#' @export There is an option to export the SPDFs above as .shp files
#' @examples
#' GlorichShp(data = hydrochemistry, convert = FALSE, export = FALSE)
#'   Returns a list of SPDFs of the same length as 'coord_sys' which have not been
#'   converted to a different coordinate reference system to the ones used in the database
#'
#' GlorichShp(data = hydrochemistry, filename = 'GlorichShp')
#'   Return a SPDF containing every point in 'hydrochemistry'
#'
#'

GlorichShp <- function(data,
                       coord_sys = list(c('NA1983', '4269'),
                                        c('WGS1984', '4326'),
                                        c('WGS 1984', '4326'),
                                        c('WGS84', '4326'),
                                        c('Gauss\x96Kr\xfcger, Zone 4, DHDN', '5678'),
                                        c('ERTS_UTM, Zone 33N, 7 digits', '25833'),
                                        c('British National Grid', '27700'),
                                        c('NatGrid NZ', '27200'),
                                        c('National Grid Australia', '5825')),
                       convert = TRUE,
                       final_CRS = '4326',
                       export = TRUE,
                       filename = NULL,
                       filepath = NULL){

  # Make the required packages available

  library(rgdal)
  library(maptools)

  # Remove entires from the database where Latitude and/or Longitude are not specified
  data <- data[!(is.na(data$Latitude) | data$Latitude==""),]
  data <- data[!(is.na(data$Longitude) | data$Longitude==""),]
  data <- data[!(is.na(data$CoordinateSystem) | data$CoordinateSystem==""),]

  # Use the list of reference systems to create subsets of the database for each system

  CRSsubset <- function(x){
    name <- x[1]
    datasubset <- subset(data, data$CoordinateSystem == name)
    # Add the EPSG code for each system to list of subsets
    data <- list(datasubset, x[2])
    return(data)
  }

  CRSdata <- lapply(coord_sys, CRSsubset)

  # Use the specified epsg codes to make each dataset a spatial object

  CRSspatial <- function(x){
    # define the dataset and EPSG code
    data <- as.data.frame(x[1])
    EPSG <- x[2]

    # Create the command to be used by CRS() for defining the coordinate system
    CRScmd <- paste('+init=EPSG:', EPSG, sep = '')

    # Convert dataframe to a spatial object and define coordinate system
    coordinates(data) <- c('Longitude', 'Latitude')
    proj4string(data) <- CRS(CRScmd)

    return(data)
  }

  CRSspatialobjects <- sapply(CRSdata, CRSspatial)

  # Function to convert each spatial object to the specified coordinate system - defaults to wgs84

  convertCRS <- function(SpatialObject, EPSG = final_CRS){
    # Define the command to be used by CRS() depending on the desired reference system
    CRScmd <- paste('+init=EPSG:', EPSG, sep = '')

    # Convert the Spatial Object
    converted <- spTransform(SpatialObject, CRS(CRScmd))

    return(converted)
  }

  # Either convert and combine the spatial objects to produce a single spatial object
  # in the desired CRS or leave the objects in the original CRS - then either export
  # as a shapefile or leave in r

  if(convert){
    # Convert each Spatial Object
    CRSconverted <- lapply(CRSspatialobjects, convertCRS)

    # Combine all Spatial objects into a single object
    CRScombined <- do.call("rbind.SpatialPointsDataFrame", CRSconverted)

    if(export){
      if(is.null(filename)){
        filename <- 'GLORICHshpExport'
      }
      if(is.null(filepath)){
        filepath <- getwd()
      }
      writeOGR(CRScombined, dsn = filepath, filename, driver = 'ESRI Shapefile', overwrite_layer = TRUE)

    }
    return(CRScombined)
  }

  # If SPDFs are not being combined but still exported, export each SPDF as a
  # separate shapefile according to a list of file names.

  if(export){
    if(is.null(filename)){
      filename <- list('NAD83', 'WGS84_1', 'WGS84_2', 'WGS84_3', 'Gauss',
                       'ERTS', 'BNG', 'NatGridNZ', 'NatGridAus')
    }
    if(is.null(filepath)){
      filepath <- getwd()
    }
    for(i in 1:(length(filename))){
      SPDF <- CRSspatialobjects[[i]]
      name <- filename[[i]]
      writeOGR(SPDF, dsn = filepath, name, driver = 'ESRI Shapefile', overwrite_layer = TRUE)
    }
  }

  # Returns the unconverted list of SPDFs if a conversion is not required
  return(CRSspatialobjects)

}
