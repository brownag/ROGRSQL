-- !preview connection=DBI::dbConnect(ROGRSQL::OGRSQL(), "lux.gpkg")

SELECT ST_Centroid(geom) FROM lux LIMIT 1;
