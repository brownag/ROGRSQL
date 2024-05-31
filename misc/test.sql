-- !preview conn=DBI::dbConnect(ROGRSQL::OGRSQL(), system.file("extdata", "lux.gpkg", package="ROGRSQL"))
SELECT ST_Centroid(geom) FROM lux LIMIT 10;
