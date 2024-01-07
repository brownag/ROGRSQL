library(terra)
library(vapour)
library(ROGRSQL)

path <- system.file("ex", "lux.shp", package = "terra")
v <- vect(path)
writeVector(v, "lux.gpkg", overwrite = TRUE)

## RSQlite+spatial functions doesn't work:
# db1 <- dbConnect(RSQLite::SQLite(), "lux.gpkg")
# dbGetQuery(db1, "SELECT ST_Centroid(geom) FROM lux LIMIT 1")

db2 <- dbConnect(ROGRSQL::OGRSQL(), "lux.gpkg")

# inspecting a spatial query result without materializing
res <- dbSendQuery(db2, "SELECT ST_Centroid(geom) FROM lux LIMIT 1")
dbColumnInfo(res)

# materialize
dbFetch(res)

# dbGetQuery() is shorthand for send/fetch/clear
