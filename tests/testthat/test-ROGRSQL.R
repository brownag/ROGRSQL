test_that("ROGRSQL works", {
  tf <- system.file("extdata", "lux.gpkg", package = "ROGRSQL")

  dbic <- new(
    "DBIConnector",
    .drv = ROGRSQL::OGRSQL(),
    .conn_args = list(dsn = tf)
  )

  twk <- DBItest::tweaks(
    # does not support boolean
    logical_return = as.integer,
    # some queries involving raw data fail to return raw
    omit_blob_tests = TRUE,
    # does not support typed date/time/timestamp/timestamp()
    timestamp_cast = function(x) paste0("('", x, "')"),
    time_typed = FALSE,
    timestamp_typed = FALSE,
    date_typed = FALSE
  )

  ctx <- DBItest::make_context(
    dbic,
    NULL,
    tweaks = twk
  )

  DBItest::test_getting_started()

  # TODO: vapour returns bigint as double not integer,
  #       numeric/character/int64 tests
  #
  #       bigint connection arg not (yet) supported
  DBItest::test_driver(skip = c("connect_bigint_integer",   # integer should always return integer, NA on overflow
                                "connect_bigint_numeric",   # numeric should work correctly
                                "connect_bigint_character", # character should produce char equivalent of numeric
                                "connect_bigint_integer64"  # integer64 should be castable to numeric/character
                                ))

  DBItest::test_connection()

  expect_warning(DBItest::test_result(skip = c(
                                "send_query_only_one_result_set",      # does not need all of these limitations
                                "fetch_no_return_value",               # CREATE TABLE fails
                                "fetch_n_progressive",                 # progressive queries not supported yet
                                "fetch_n_more_rows",                   # n more rows not supported yet
                                # "fetch_n_zero_rows",                 # vapour does not support limit_n==0
                                "clear_result_return_statement",       # CREATE TABLE fails
                                "clear_result_return_query_arrow",     # CREATE TABLE fails
                                "cannot_clear_result_twice_statement", # CREATE TABLE fails
                                "get_query_n_more_rows",               # n more rows not supported yet
                                # "get_query_n_zero_rows",             # vapour does not support limit_n==0
                                "send_statement_trivial",
                                "send_statement_syntax_error",
                                "send_statement_result_valid",
                                "send_statement_only_one_result_set",
                                "send_statement_immediate",
                                "execute_atomic",
                                "execute_immediate",
                                "data_type_create_table",
                                # "data_numeric",                      # UNION queries w/ NULL return character
                                # "data_logical",                      # logical returns as integer
                                "data_character",
                                # "data_raw",                          # UNION queries w/ NULL return NA
                                # "data_timestamp",                    # timestamp() not supported
                                # "data_date_typed",
                                # "data_date_current_typed",
                                # "data_timestamp_typed",
                                # "data_timestamp_current_typed",
                                "data_64_bit_numeric_warning",         # bigint handling/warning not supported yet
                                "data_64_bit_lossless")))
})

## additional simpler tests added to track incremental fixes
path <- system.file("extdata", "lux.gpkg", package = "ROGRSQL")
db <- dbConnect(ROGRSQL::OGRSQL(), path)

## See supported field types:
##   https://gdal.org/api/vector_c_api.html#_CPPv412OGRFieldType
test_that("data_numeric", {
  x <- dbGetQuery(db, "SELECT NULL as a, NULL as b, 1 as id UNION SELECT 1.5 as a, -100.5 as b, 2 as id")
  expect_true(is.numeric(x[[1]]) && is.numeric(x[[2]]))
})

test_that("data_logical", {
  # logicals are returned as integer
  y <- dbGetQuery(db, "SELECT CAST(1 as boolean) AS a")
  expect_true(is.integer(y[[1]]))
})

test_that("data_raw", {
  # this works
  z <- dbGetQuery(db, "SELECT X'00' as a")
  expect_true(is.list(z) && is.raw(z[[1]]))

  # this does not
  z2 <- dbGetQuery(db, "SELECT NULL as a, 1 as id UNION SELECT X'00' as a, 2 as id")
  expect_true(all(is.na(z2[[1]])))
})
