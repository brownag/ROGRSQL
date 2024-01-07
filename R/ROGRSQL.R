#' DBI Driver for GDAL OGRSQL
#'
#' @keywords internal
#' @export
#' @import DBI
#' @import methods
setClass("GDALOGRSQLDriver", contains = "DBIDriver")

#' @importFrom dbplyr dbplyr_edition
#' @export
dbplyr_edition.GDALOGRSQLConnection <- function(con) 2L

#' DBI Connection for GDAL OGRSQL
#'
#' @export
#' @keywords internal
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

#' @export
#' @rdname GDALOGRSQLDriver-class
setMethod("dbUnloadDriver", "GDALOGRSQLDriver", function(drv, ...) {
  TRUE
})

setMethod("show", "GDALOGRSQLDriver", function(object) {
  cat("<GDALOGRSQLDriver>\n")
})

#' @rdname OGRSQL
#' @export
OGRSQL <- function() {
  new("GDALOGRSQLDriver")
}

#' GDAL OGR SQL Driver
#'
#' @param drv An object created by \code{OGRSQL())}
#' @param dsn character. Path to OGR-compatible data source.
#' @param ... Additional arguments
#' @rdname OGRSQL
#' @export
#' @examples
#' \dontrun{
#' db <- dbConnect(ROGRSQL::OGRSQL(), "nc.gpkg")
#' dbSendQuery(db, "SELECT * FROM mtcars WHERE cyl == 4")
#' }
setMethod("dbConnect", "GDALOGRSQLDriver", function(drv, dsn, ...) {
  conn <- new("GDALOGRSQLConnection", dsn = dsn, ref = new.env(), ...)
  conn@ref$connected <- TRUE
  conn
})

#' Disconnect a GDALOGRSQLConnection
#' @param conn A GDALOGRSQLConnection
#' @param ... Additional arguments (not used)
#' @export
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
setMethod("dbGetInfo", "GDALOGRSQLDriver", function(dbObj, ...) {
  list(driver.version = utils::packageVersion("ROGRSQL"),
       client.version = gsub("GDAL ([0-9\\.\\-]+), .*$", "\\1", vapour::vapour_gdal_version()))
})

#' Get GDALOGRSQLConnection information
#' @param dbObj A GDALOGRSQLConnection
#' @param ... Additional arguments (not used)
#' @export
setMethod("dbGetInfo", "GDALOGRSQLConnection", function(dbObj, ...) {
  list(driver.version = utils::packageVersion("ROGRSQL"),
       db.version = gsub("GDAL ([0-9\\.\\-]+), .*$", "\\1", vapour::vapour_gdal_version()),
       dbname = dbObj@dsn,
       username = NA_character_,
       host = NA_character_,
       port = NA_integer_)
})


#' GDALOGRSQLResult class
#'
#' @keywords internal
#' @export
setClass("GDALOGRSQLResult",
         contains = "DBIResult",
         slots = list(sql = "character",
                      conn = "GDALOGRSQLConnection",
                      lyr = "list")
)

#' Send a query to GDALOGRSQLDriver
#'
#' @param conn A GDALOGRSQLConnection Object
#' @param statement An OGRSQL query to execute.
#' @param ... Additional arguments
#' @export
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
      dbFetch(res, n = n, geom = geom)
    else
      dbFetch(res, n = NULL, geom = geom)
  })
})

#' Clear Results
#'
#' @param res A GDALOGRSQLResult Object
#' @param ... Additional arguments
#' @export
setMethod("dbClearResult", "GDALOGRSQLResult", function(res, ...) {
  # free resources
  if (is.null(res@conn@ref$result)) {
    warning("GDALOGRSQLResult already cleared")
  }
  res@conn@ref$result <- NULL
  invisible(TRUE)
})

.validate_n <- function(n) {
  if (!is.null(n) && (n == Inf || n == -1)) {
    n <- NULL
  }

  if (!is.null(n) && (any(n < 0) || any(n != as.integer(n)))) {
    stop("n (", paste0(n, collapse = ","), ") should be a positive integer")
  }
  n
}

#' Retrieve records from OGRSQL query

#' @param res A GDALOGRSQLResult Object
#' @param n Number of records to fetch. Default `NULL` for all records.
#' @param ... Additional arguments
#' @param geom character. Either `"json"`, `"gml"`, `"kml"`, `"wkt"` (default) or `"hex"`
#' @param fid logical. Keep feature ID? Default: `FALSE`
#' @export
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

  # TODO: bigint?
  f <- vapour::vapour_read_fields(src, layer = lyr, sql = sql, limit_n = n)

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
  out
})

#' Find the database data type associated with an R object
#' @param dbObj GDALOGRConnection
#' @param obj An R object
#' @param ... Additional arguments
#' @export
#' @importFrom RSQLite dbDataType SQLite
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
setMethod("dbHasCompleted", "GDALOGRSQLResult", function(res, ...) {
  is.null(res@conn@ref$result)
})

#### https://cran.r-project.org/web/packages/DBI/vignettes/backend.html

#' Safely quote strings and identifiers to avoid SQL injection attacks
#' @rdname GDALOGRSQLConnection-class
#' @export
setMethod("dbQuoteIdentifier", c("GDALOGRSQLConnection", "character"), function(conn, x, ...) {
  # borrowed from https://github.com/r-dbi/RSQLite
  if (any(is.na(x))) {
    stop("Cannot pass `NA` to dbQuoteIdentifier()", call. = FALSE)
  }
  x <- gsub("`", "``", enc2utf8(x))
  if (length(x) == 0L) {
    DBI::SQL(character(), names = names(x))
  } else {
    DBI::SQL(paste("`", x, "`", sep = ""), names = names(x))
  }
})

#
# dbWriteTable() creates a database table given an R dataframe. I’d recommend using the functions prefixed with sql in this package to generate the SQL. These functions are still a work in progress so please let me know if you have problems.
#
# dbReadTable(): a simple wrapper around SELECT * FROM table. Use dbQuoteIdentifer() to safely quote the table name and prevent mismatches between the names allowed by R and the database.
#
# dbListTables() and dbExistsTable() let you determine what tables are available. If not provided by your database’s API, you may need to generate sql that inspects the system tables.
setMethod("dbListTables", "GDALOGRSQLConnection", function(conn) {
  vapour::vapour_layer_names(conn@dsn)
})

#' Get fields are available in a given table.
#' @param conn GDALOGRSQLConnection
#' @param name character. Table name.
#' @export
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
setMethod("dbIsValid", "GDALOGRSQLConnection", function(dbObj, ...) {
  isTRUE(dbObj@ref$connected)
})

#' Determine if a result set is open or closed
#' @param dbObj GDALOGRSQLResult
#' @param ... Additional arguments
#' @export
setMethod("dbIsValid", "GDALOGRSQLResult", function(dbObj, ...) {
  is.null(dbObj@conn@ref$result)
})

#' Get the issued query as a character value
#' @param res GDALOGRSQLResult
#' @export
setMethod("dbGetStatement", "GDALOGRSQLResult", function(res) {
  res@sql
})

#' Get names and types of the result set’s columns.
#' @param res GDALOGRSQLResult
#' @export
setMethod("dbColumnInfo", "GDALOGRSQLResult", function(res) {
  src <- res@conn@dsn
  fid <-  data.frame(name = "fid", type = "integer")
  g <- vapour::vapour_geom_name(src, layer = res@lyr$layer, sql = res@sql)
  geom <- data.frame(name = g,
                     type = rep("list", length(g)))
  rbind(fid, geom, data.frame(name = names(res@lyr$fields),
                              type = .remap_types(as.character(res@lyr$fields))))
})

.remap_types <- function(x) {
  y <- c("double", "character", "integer")
  names(y) <- c("OFTReal", "OFTString", "OFTInteger64")
  y[x]
}

#' Get number of rows altered in a INSERT/UPDATE query
#' @param res GDALOGRSQLResult
#' @export
setMethod("dbGetRowsAffected", "GDALOGRSQLResult", function(res) {
  0
})

#' Get number of rows returned in a SELECT query
#' @param res GDALOGRSQLResult
#' @export
setMethod("dbGetRowCount", "GDALOGRSQLResult", function(res) {
 # TODO: was not originally needed, needed when n passed through to dbFetch
 i <- suppressWarnings(as.numeric(gsub(".*LIMIT (\\d+)$", "\\1", as.character(res@sql))))
 if (is.na(i) || !is.numeric(i)) {
   return(length(vapour::vapour_read_fids(res@conn@dsn, res@lyr$layer, sql = res@sql)))
 }
 as.integer(i)
})

# dbBind() allows using parametrised queries. Take a look at sqlInterpolate() and sqlParseVariables() if your SQL engine doesn’t offer native parametrised queries.

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
