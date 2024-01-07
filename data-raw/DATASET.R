# create luxembourg geopackage from terra shapefile
library(terra)
terra::writeVector(terra::vect(system.file("ex", "lux.shp", package = "terra")),
                   "inst/extdata/lux.gpkg")
