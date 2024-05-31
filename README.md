
<!-- README.md is generated from README.Rmd. Please edit that file -->

# {ROGRSQL}

<!-- badges: start -->

[![R-CMD-check](https://github.com/brownag/ROGRSQL/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/brownag/ROGRSQL/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/license/MIT/)
[![ROGRSQL
Manual](https://img.shields.io/badge/docs-HTML-informational)](https://humus.rocks/ROGRSQL/)
<!-- badges: end -->

The goal of {ROGRSQL} is to provide a basic proof-of-concept for limited
DBI-compatible queries with the [Geospatial Data Abstraction Library
(GDAL)](https://gdal.org/)
[‘OGRSQL’](https://gdal.org/user/ogr_sql_dialect.html) dialect.

## Installation

You can install the development version of {ROGRSQL} like so:

``` r
remotes::install_github("brownag/ROGRSQL")
```

## Example

Here is a basic demonstration using a sample dataset from the {terra}
package.

The shapefile is written to a new GeoPackage, and the GeoPackage is used
as the data source which is queried using GDAL OGRSQL.

``` r
library(ROGRSQL)

path <- sample_gpkg_path()

db <- dbConnect(ROGRSQL::OGRSQL(), path)
dbGetQuery(db, "SELECT ST_Centroid(geom) FROM lux LIMIT 2")
#>                           ST_Centroid(geom)
#> 1  POINT (6.0090815315983 50.0706361949086)
#> 2 POINT (6.12742481671626 49.8661396602439)
```

We can make more complex queries, but there are limitations in OGRSQL
with the way that geometries versus attributes are interpreted.

``` r
db <- dbConnect(ROGRSQL::OGRSQL(), path)
dbGetQuery(db, "SELECT ST_Centroid(geom), ST_ConvexHull(geom) FROM lux LIMIT 2")
#>                           ST_Centroid(geom) ST_ConvexHull(geom)
#> 1  POINT (6.0090815315983 50.0706361949086)        00, 01, ....
#> 2 POINT (6.12742481671626 49.8661396602439)        00, 01, ....
```

Note that in above case two geometry columns are calculated in the
result, but only one is selected by GDAL as the *main* geometry column
for the layer/query result. The result of `ST_ConvexHull(geom)` is then
read as if it were a non-spatial attribute.

# RStudio Interface and RMarkdown

You can use the `OGRSQL()` DBI driver to dynamically preview your
results in the RStudio SQL editor. This simply requires using a special
comment at the top of your SQL script, like so:

``` sql
-- !preview conn=DBI::dbConnect(ROGRSQL::OGRSQL(), ROGRSQL::sample_gpkg_path())

SELECT ST_Centroid(geom) FROM lux LIMIT 1;
```

<div class="knitsql-table">

| ST_Centroid(geom)                        |
|:-----------------------------------------|
| POINT (6.0090815315983 50.0706361949086) |

1 records

</div>

In an {rmarkdown} code chunk, you can similarly do:
`--| connection=DBI::dbConnect(ROGRSQL::OGRSQL(), ROGRSQL::sample_gpkg_path())`.

# Constructing OGRSQL Queries with {dbplyr}

With `ROGRSQL::OGRSQL()` we are able to make full use of the OGRSQL
dialect. This is unlike using the {RSQLite} `SQLite()` driver you might
use for querying a GeoPackage, or using `sf::st_read(..., query=)` for
other OGR source. For example, we can use spatial functions such as
`ST_Centroid()` within our query to the data source. There are several
other OGRSQL specific conveniences which are described in greater detail
in the GDAL documentation: <https://gdal.org/user/ogr_sql_dialect.html>

Since the *GDALOGRSQLConnection* object `db` encapsulates a
*DBIConnection*, we can utilize {dbplyr} to construct SQL queries
“lazily” with R code. Once the query has been built, we can execute it
with `dbplyr::collect()`. Note that the same functionality does not work
with the standard {RSQLite} driver.

``` r
library(ROGRSQL)
library(dplyr, warn.conflicts = FALSE)
library(dbplyr, warn.conflicts = FALSE)

db1 <- dbConnect(RSQLite::SQLite(), system.file("extdata", "lux.gpkg", package = "ROGRSQL"))
db2 <- dbConnect(ROGRSQL::OGRSQL(), system.file("extdata", "lux.gpkg", package = "ROGRSQL"))

# error with ST_Centroid
tbl(db1, "lux") |> 
  group_by(NAME_1) |> 
  filter(ID_2 %% ID_1 == 0) |>
  summarize(ST_Centroid(geom)) 
#> Error in `collect()`:
#> ! Failed to collect lazy table.
#> Caused by error:
#> ! no such function: ST_Centroid
```

``` r

# works
tbl(db2, "lux") |> 
  group_by(NAME_1) |>
  filter(ID_2 %% ID_1 == 0) |>
  ungroup() |>
  summarize(ST_Centroid(geom)) -> res

res |> 
  collect()
#> Note: method with signature 'GDALOGRSQLConnection#character' chosen for function 'dbQuoteIdentifier',
#>  target signature 'GDALOGRSQLConnection#SQL'.
#>  "DBIConnection#SQL" would also be valid
#> Warning: Only first 8 results retrieved. Use n = Inf to retrieve all.
#> # A tibble: 8 × 1
#>   `ST_Centroid(geom)`                      
#>   <chr>                                    
#> 1 POINT (6.0090815315983 50.0706361949086) 
#> 2 POINT (6.12742481671626 49.8661396602439)
#> 3 POINT (5.88650246888354 49.8001378080874)
#> 4 POINT (6.16508092561211 49.9288611138491)
#> 5 POINT (5.91454519617064 49.9389181305846)
#> 6 POINT (6.37844920946634 49.7851091638985)
#> 7 POINT (6.34639476753763 49.6874179035032)
#> 8 POINT (6.02381557605546 49.5233098534297)
```

``` r

# inspect generated query
show_query(res)
#> <SQL>
#> SELECT ST_Centroid(`geom`) AS `ST_Centroid(geom)`
#> FROM (
#>   SELECT `lux`.*
#>   FROM `lux`
#>   WHERE ((`ID_2` % `ID_1`) = 0.0)
#> ) `q01`
```

# Similar work

I whipped this up as a proof-of-concept to better understand the
workings of the GDAL API (here accessed via the excellent
[{vapour}](https://github.com/hypertidy/vapour/) package by @mdsumner),
as well as to learn more about developing {DBI}/{dbplyr} backends.

I was exploring this topic in the context of adding some new
OGRSQL-specific functionality to
[{gpkg}](https://github.com/brownag/gpkg/) which would utilize {vapour}
rather than {terra}. It does seem as if there may not currently be a
package on CRAN that provides a modern (dbplyr \>= 2.0.0), fully
DBI-compatible interface tailored to the GDAL OGRSQL dialect. So,
perhaps there is a space for a package like this, but it is also not
entirely clear it is “needed”. I will point out that much of the basic
SQL evaluation/spatial queries etc. can be readily achieved with
{sf}+{DBI} or {terra}.

After getting the basics minimally working, and thinking about pushing
the repo up to GitHub, I realized that folks have tread in this space
many years prior to me, namely: @mdsumner
(<https://github.com/mdsumner/RGDALSQL>); and @etiennebr
(<https://github.com/r-spatial/sfdbi>). These packages both appear to be
excellent implementations, and mostly working, though they have not been
updated recently, and there have been some further developments in the
{dbplyr} space in the last several years.

It is not terribly likely I will spend much more time developing this
package, but if you are interested in seeing this functionality made
more available get in contact with me. Feel free to ask questions/post
issues in the [issue
tracker](https://github.com/brownag/ROGRSQL/issues/).
