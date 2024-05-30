-- !preview conn=DBI::dbConnect(ROGRSQL::OGRSQL(), system.file("extdata", "lux.gpk", package="ROGRSQL"))
SELECT ST_Centroid(geom) FROM lux LIMIT 1;
