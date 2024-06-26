#' DBI Driver for GDAL OGRSQL
#'
#' @keywords internal
#' @export
#' @import DBI
#' @import methods
# GDALOGRSQLDriver ----
setClass("GDALOGRSQLDriver", contains = "DBIDriver")

#' @rdname OGRSQL
#' @export
OGRSQL <- function() {
  new("GDALOGRSQLDriver")
}

#' @importFrom dbplyr dbplyr_edition
#' @export
dbplyr_edition.GDALOGRSQLConnection <- function(con) 2L

#' DBI Connection for GDAL OGRSQL
#'
#' @export
#' @keywords internal
# GDALOGRSQLConnection ----
setClass("GDALOGRSQLConnection",
         contains = "DBIConnection",
         slots = list(
           dsn = "character",
           ptr = "externalptr",
           ref = "environment",
           bigint = "character"
         ),
         validity = function(object) TRUE
)

#' GDALOGRSQLResult class
#'
#' @keywords internal
#' @export
# GDALOGRSQLResult ----
setClass("GDALOGRSQLResult",
         contains = "DBIResult",
         slots = list(sql = "character",
                      conn = "GDALOGRSQLConnection",
                      lyr = "list")
)

# show<GDALOGRSQLDriver> ----
setMethod("show", "GDALOGRSQLDriver", function(object) {
  cat("<GDALOGRSQLDriver>\n")
})

#' @export
#' @rdname GDALOGRSQLDriver-class
# dbUnloadDriver<GDALOGRSQLDriver> ----
setMethod("dbUnloadDriver", "GDALOGRSQLDriver", function(drv, ...) {
  TRUE
})

#' GDAL OGR SQL Driver
#'
#' @param drv An object created by \code{OGRSQL())}
#' @param dsn character. Path to OGR-compatible data source.
#' @param bigint character. Return type for _OFTInteger64_ fields. One of: `"integer"`, `"character"`, `"numeric"`, `"integer64"` (default)
#' @param ... Additional arguments
#' @rdname OGRSQL
#' @export
#' @examples
#' \dontrun{
#' db <- dbConnect(ROGRSQL::OGRSQL(), "nc.gpkg")
#' dbSendQuery(db, "SELECT * FROM mtcars WHERE cyl == 4")
#' }
# dbConnect<GDALOGRSQLDriver> ----
setMethod("dbConnect", "GDALOGRSQLDriver", function(drv, dsn, bigint = "integer64", ...) {
  conn <- new("GDALOGRSQLConnection", dsn = dsn, ref = new.env(), ...)
  conn@ref$connected <- TRUE
  conn@bigint <- match.arg(bigint, choices = c("integer", "character", "numeric", "integer64"))
  conn
})

#' Disconnect a GDALOGRSQLConnection
#' @param conn A GDALOGRSQLConnection
#' @param ... Additional arguments (not used)
#' @export
# dbDisconnect<GDALOGRSQLConnection> ----
setMethod("dbDisconnect", "GDALOGRSQLConnection", function(conn, ...) {
  if (!conn@ref$connected) {
    warning("GDALOGRSQLConnection is already disconnected", call. = FALSE)
  }
  if (!is.null(conn@ref$result)) {
    warning("GDALOGRSQLConnection results have not been cleared", call. = FALSE)
  }
  conn@ref$connected <- FALSE
  invisible(TRUE)
})

#' Get GDALOGRSQLDriver driver and GDAL version information
#' @param dbObj A GDALOGRSQLDriver
#' @param ... Additional arguments (not used)
#' @export
#' @importFrom utils packageVersion
# dbGetInfo<GDALOGRSQLDriver> ----
setMethod("dbGetInfo", "GDALOGRSQLDriver", function(dbObj, ...) {
  list(driver.version = utils::packageVersion("ROGRSQL"),
       client.version = gsub("GDAL ([0-9\\.\\-]+), .*$", "\\1", vapour::vapour_gdal_version()))
})

#' Get GDALOGRSQLConnection information
#' @param dbObj A GDALOGRSQLConnection
#' @param ... Additional arguments (not used)
#' @export
# dbGetInfo<GDALOGRSQLConnection> ----
setMethod("dbGetInfo", "GDALOGRSQLConnection", function(dbObj, ...) {
  list(driver.version = utils::packageVersion("ROGRSQL"),
       db.version = gsub("GDAL ([0-9\\.\\-]+), .*$", "\\1", vapour::vapour_gdal_version()),
       dbname = dbObj@dsn,
       username = NA_character_,
       host = NA_character_,
       port = NA_integer_)
})

#' Send a query to GDALOGRSQLDriver
#'
#' @param conn A GDALOGRSQLConnection Object
#' @param statement An OGRSQL query to execute.
#' @param ... Additional arguments
#' @export
# dbSendQuery<GDALOGRSQLConnection> ----
setMethod("dbSendQuery", "GDALOGRSQLConnection", function(conn, statement, ...) {
  i <- vapour::vapour_layer_info(conn@dsn, vapour::vapour_layer_names(conn@dsn)[1])

  if (!conn@ref$connected) {
    stop("GDALOGRSQLConnection is no longer connected")
  }

  if (length(statement) == 0 || is.na(statement)) {
    stop("invalid statement: ", statement)
  }


  check_syntax <- .withWarnings(try(vapour::vapour_read_fids(conn@dsn, sql = statement, limit_n = 1), silent = TRUE))
  if (inherits(check_syntax$value, 'try-error')) {
    sapply(check_syntax$warnings, message, "\n")
    stop(check_syntax$value[1])
  }

  if (!is.null(conn@ref$result)) {
    dbClearResult(conn@ref$result)
    stopifnot(is.null(conn@ref$result))
  }

  rs <- new("GDALOGRSQLResult",
            sql = statement,
            conn = conn,
            lyr = i)

  # on.exit(dbClearResult(rs), add = TRUE)

  # if (!is.null(params)) {
  #   dbBind(rs, params)
  # }
  # on.exit(NULL, add = FALSE)

  conn@ref$result <- rs
  rs@conn <- conn
  rs
})

#' Send and Fetch an OGRSQL Query
#' @param conn A GDALOGRSQLConnection Object
#' @param statement An OGRSQL query to execute.
#' @param ... Additional arguments to `dbSendQuery()`
#' @param geom character. Either `"json"`, `"gml"`, `"kml"`, `"wkt"` (default) or `"hex"`
#' @export
# dbGetQuery<GDALOGRSQLConnection> ----
setMethod("dbGetQuery", c("GDALOGRSQLConnection", "character"),
          function(conn, statement, ..., geom = "wkt") {
  if (!is.null(conn@ref$result)) {
    dbClearResult(conn@ref$result)
  }
  x <- list(...)
  n <- NULL
  if (!is.null(x[["n"]])) {
    n <- .validate_n(x[["n"]])
  }
  suppressWarnings({
    res <- dbSendQuery(conn, statement, ...)
    if (!is.null(n))
      out <- dbFetch(res, n = n, geom = geom)
    else
      out <- dbFetch(res, n = NULL, geom = geom)
  })
  dbClearResult(res)
  out
})

#' Clear Results
#'
#' @param res A GDALOGRSQLResult Object
#' @param ... Additional arguments
#' @export
# dbClearResult<GDALOGRSQLResult> ----
setMethod("dbClearResult", "GDALOGRSQLResult", function(res, ...) {
  # free resources
  if (is.null(res@conn@ref$result)) {
    warning("GDALOGRSQLResult already cleared")
  }
  res@conn@ref$result <- NULL
  invisible(TRUE)
})


#' Retrieve records from OGRSQL query

#' @param res A GDALOGRSQLResult Object
#' @param n Number of records to fetch. Default `NULL` for all records.
#' @param ... Additional arguments
#' @param geom character. Either `"json"`, `"gml"`, `"kml"`, `"wkt"` (default) or `"hex"`
#' @param fid logical. Keep feature ID? Default: `FALSE`
#' @export
#' @importFrom utils capture.output type.convert
# dbFetch<GDALOGRSQLResult> ----
setMethod("dbFetch", "GDALOGRSQLResult", function(res, n = NULL, ..., geom = "wkt", fid = FALSE) {

  if (!res@conn@ref$connected) {
    stop("GDALOGRSQLConnection is no longer connected")
  }

  if (is.null(res@conn@ref$result)) {
    stop("GDALOGRSQLResult has been cleared")
  }

  n <- .validate_n(n)

  src <- res@conn@dsn
  lyr <- res@lyr$layer
  sql <- res@sql

  # TODO: default name?
  # TODO: more than one geometry in result?
  gnm <- vapour::vapour_geom_name(src, layer = lyr, sql = sql)

  # vapour produces Rprintf output for extent-less results
  utils::capture.output({
    lin <- vapour::vapour_layer_info(src, layer = lyr, sql = sql)
  })

  if (!is.null(n) && n > lin$count) {
    n <- lin$count
  }

  nzero <- FALSE
  if (!is.null(n) && n == 0) {
    n <- 1
    nzero <- TRUE
  }

  f <- vapour::vapour_read_fields(src, layer = lyr, sql = sql, limit_n = n)

  nf <- names(f)
  f <- lapply(seq(f), function(i) {
    if (res@conn@bigint %in% c("character", "integer64") &&
        lin$fields[i] == "OFTInteger64" &&
        any(abs(f[[i]]) > .Machine$integer.max / 2)) {
      if (!requireNamespace("bit64"))
        stop("package 'bit64' is required for connections with bigint='integer64' or bigint='character'", call. = FALSE)
      f2 <- bit64::as.integer64(f[[i]])
      if (res@conn@bigint == "character") {
        as.character(f2)
      } else {
        f2
      }
    } else {
      switch(res@conn@bigint,
             `integer` = as.integer(f[[i]]),
             f[[i]])
    }
  })
  names(f) <- nf

  # TODO: binary geometry
  if (tolower(trimws(geom)) %in% c("json", "gml", "kml", "wkt")) {
   g <- vapour::vapour_read_geometry_text(src, layer = lyr, sql = sql, textformat = geom, limit_n = n)
  }

  # TODO: optional FID? custom name?
  out <- data.frame(fid = vapour::vapour_read_fids(src, layer = lyr, sql = sql, limit_n = n))

  if (length(g) > 0 && length(gnm) > 0) {
    out[[gnm]] <- unlist(g)
  } else if (length(gnm) > 0) {
    out[[gnm]] <- list()
  }

  if (length(f) > 0) {
    for (i in seq(f)) {
      if (is.list(f[[i]])) {
        f[[i]] <- I(f[[i]])
      }
      fff <- data.frame(f[[i]])
      colnames(fff) <- names(f[i])
      out <- cbind(out, fff)
    }
  }

  if (!fid) {
    out$fid <- NULL
  }

  # protect raw values during conversion
  out.raw <- sapply(out, function(o) is.list(o) || is.raw(o) || bit64::is.integer64(o))
  out.raw[lin$fields == "OFTInteger64"] <- TRUE
  out2 <- utils::type.convert(out, as.is = TRUE)
  out2[out.raw] <- out[out.raw]

  if (nzero) {
    return(out2[0, , drop = FALSE])
  }
  out2
})

#' Find the database data type associated with an R object
#' @param dbObj GDALOGRConnection
#' @param obj An R object
#' @param ... Additional arguments
#' @export
#' @importFrom RSQLite dbDataType SQLite
# dbDataType<GDALOGRSQLConnection> ----
setMethod("dbDataType", "GDALOGRSQLConnection", function(dbObj, obj, ...) {
  # TODO: customize for OGRSQL
  res <- RSQLite::dbDataType(RSQLite::SQLite(), obj)
  # print(list(input = obj, output = res))
  res
})

#' Check if rows remain to be fetched
#'
#' @param res A GDALOGRSQLResult Object
#' @param ... Additional arguments
#' @export
# dbHasCompleted<GDALOGRSQLResult> ----
setMethod("dbHasCompleted", "GDALOGRSQLResult", function(res, ...) {
  is.null(res@conn@ref$result)
})

#### https://cran.r-project.org/web/packages/DBI/vignettes/backend.html

#' Safely quote strings and identifiers to avoid SQL injection attacks
#' @rdname GDALOGRSQLConnection-class
#' @export
# dbQuoteIdentifier<GDALOGRSQLConnection> ----
setMethod("dbQuoteIdentifier", c("GDALOGRSQLConnection", "character"), function(conn, x, ...) {
  # borrowed from https://github.com/r-dbi/RSQLite
  if (any(is.na(x))) {
    stop("Cannot pass `NA` to dbQuoteIdentifier()", call. = FALSE)
  }
  x <- gsub("`", "``", enc2utf8(x))
  if (length(x) == 0L) {
    DBI::SQL(character(), names = names(x))
  } else {
    DBI::SQL(ifelse(x == "*", x, paste("`", x, "`", sep = "")), names = names(x))
  }
})

#
# dbWriteTable() creates a database table given an R dataframe. I’d recommend using the functions prefixed with sql in this package to generate the SQL. These functions are still a work in progress so please let me know if you have problems.
#
# dbReadTable(): a simple wrapper around SELECT * FROM table. Use dbQuoteIdentifer() to safely quote the table name and prevent mismatches between the names allowed by R and the database.
#
# dbListTables() and dbExistsTable() let you determine what tables are available. If not provided by your database’s API, you may need to generate sql that inspects the system tables.
# dbListTables<GDALOGRSQLConnection> ----
setMethod("dbListTables", "GDALOGRSQLConnection", function(conn) {
  vapour::vapour_layer_names(conn@dsn)
})

#' Get fields are available in a given table.
#' @param conn GDALOGRSQLConnection
#' @param name character. Table name.
#' @export
# dbListFields<GDALOGRSQLConnection> ----
setMethod("dbListFields", c("GDALOGRSQLConnection", "character"), function(conn, name) {
  c(
    "fid",
    vapour::vapour_geom_name(conn@dsn, layer = name),
    names(vapour::vapour_report_fields(conn@dsn, layer = name))
  )
})

# dbRemoveTable() wraps around DROP TABLE. Start with SQL::sqlTableDrop().

# dbBegin(), dbCommit() and dbRollback(): implement these three functions to provide basic transaction support. This functionality is currently not tested in the DBItest package.

#' Determine if connection is open or closed
#' @param dbObj GDALOGRConnection
#' @param ... Additional arguments
#' @export
# dbIsValid<GDALOGRSQLConnection> ----
setMethod("dbIsValid", "GDALOGRSQLConnection", function(dbObj, ...) {
  isTRUE(dbObj@ref$connected)
})

#' Determine if a result set is open or closed
#' @param dbObj GDALOGRSQLResult
#' @param ... Additional arguments
#' @export
# dbIsValid<GDALOGRSQLResult> ----
setMethod("dbIsValid", "GDALOGRSQLResult", function(dbObj, ...) {
  !is.null(dbObj@conn@ref$result)
})

#' Get the issued query as a character value
#' @param res GDALOGRSQLResult
#' @export
# dbGetStatement<GDALOGRSQLResult> ----
setMethod("dbGetStatement", "GDALOGRSQLResult", function(res) {
  res@sql
})

#' Get names and types of the result set’s columns.
#' @param res GDALOGRSQLResult
#' @export
# dbColumnInfo<GDALOGRSQLResult> ----
setMethod("dbColumnInfo", "GDALOGRSQLResult", function(res) {
  src <- res@conn@dsn
  fid <-  data.frame(name = "fid", type = "integer")
  g <- vapour::vapour_geom_name(src, layer = res@lyr$layer, sql = res@sql)
  geom <- data.frame(name = g,
                     type = rep("list", length(g)))
  rbind(fid, geom, data.frame(name = names(res@lyr$fields),
                              type = .remap_types(res@conn,
                                                  as.character(res@lyr$fields))))
})

#' Get number of rows altered in a INSERT/UPDATE query
#' @param res GDALOGRSQLResult
#' @export
# dbGetRowsAffected<GDALOGRSQLResult> ----
setMethod("dbGetRowsAffected", "GDALOGRSQLResult", function(res) {
  0
})

#' Get number of rows returned in a SELECT query
#' @param res GDALOGRSQLResult
#' @export
# dbGetRowCount<GDALOGRSQLResult> ----
setMethod("dbGetRowCount", "GDALOGRSQLResult", function(res) {
 # TODO: was not originally needed, needed when n passed through to dbFetch
 i <- suppressWarnings(as.numeric(gsub(".*LIMIT (\\d+)$", "\\1", as.character(res@sql))))
 if (is.na(i) || !is.numeric(i)) {
   return(length(vapour::vapour_read_fids(res@conn@dsn, res@lyr$layer, sql = res@sql)))
 }
 as.integer(i)
})

# dbBind() allows using parametrised queries. Take a look at sqlInterpolate() and sqlParseVariables() if your SQL engine doesn’t offer native parametrised queries.


.validate_n <- function(n) {
  if ((!is.null(n) && (is.na(n) || n == Inf || n == -1))) {
    n <- NULL
  }

  if (!is.null(n) && (any(n < 0) || any(n != as.integer(n)))) {
    stop("n (", paste0(n, collapse = ","), ") should be a positive integer")
  }
  n
}

.remap_types <- function(conn, x) {
  y <- c("double", "character", "integer",
         conn@bigint, "raw", "character",
         "character", "character")
  names(y) <- c("OFTReal", "OFTString", "OFTInteger",
                "OFTInteger64", "OFTBinary", "OFTDate",
                "OFTTime", "OFTDatetime")
  y[x]
}

# author: luke tierney
#  <https://stat.ethz.ch/pipermail/r-help/2004-June/052132.html>
.withWarnings <- function(expr) {
  myWarnings <- NULL
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, list(w))
    invokeRestart("muffleWarning")
  }
  val <- withCallingHandlers(expr, warning = wHandler)
  list(value = val, warnings = myWarnings)
}

#' Sample 'Luxembourg' Geopackage
#'
#' This is a Geopackage file created from the `"lux.shp"` test dataset in the `{terra}` R package.
#'
#' @return character. Path to ROGRSQL package `"extdata"` folder `"lux.gpkg"` file.
#' @references Hijmans R (2024). _terra: Spatial Data Analysis_. R package version 1.7-78, https://rspatial.github.io/terra/, <https://rspatial.org/>.
#'
#' @export
#' @examples
#' sample_gpkg_path()
sample_gpkg_path <- function() {
  system.file("extdata", "lux.gpkg", package = "ROGRSQL")
}
