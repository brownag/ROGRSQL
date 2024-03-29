% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ROGRSQL.R
\name{dbGetQuery,GDALOGRSQLConnection,character-method}
\alias{dbGetQuery,GDALOGRSQLConnection,character-method}
\title{Send and Fetch an OGRSQL Query}
\usage{
\S4method{dbGetQuery}{GDALOGRSQLConnection,character}(conn, statement, ..., geom = "wkt")
}
\arguments{
\item{conn}{A GDALOGRSQLConnection Object}

\item{statement}{An OGRSQL query to execute.}

\item{...}{Additional arguments to \code{dbSendQuery()}}

\item{geom}{character. Either \code{"json"}, \code{"gml"}, \code{"kml"}, \code{"wkt"} (default) or \code{"hex"}}
}
\description{
Send and Fetch an OGRSQL Query
}
