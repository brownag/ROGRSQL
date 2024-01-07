test_that("ROGRSQL works", {
  tf <- system.file("extdata", "lux.gpkg", package = "ROGRSQL")
  DBItest::make_context(new(
    "DBIConnector",
    .drv = ROGRSQL::OGRSQL(),
    .conn_args = list(dsn = tf)
  ), NULL)

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

  expect_warning(DBItest::test_result(skip = c("send_query_only_one_result_set", # does not need all of these limitations
                                "fetch_no_return_value",          # CREATE TABLE fails
                                "fetch_n_progressive",            # progressive queries not supported yet
                                "fetch_n_more_rows",              # n more rows not supported yet
                                "fetch_n_zero_rows",              # vapour does not support limit_n==0
                                "clear_result_return_statement",  # CREATE TABLE fails
                                "clear_result_return_query_arrow", # CREATE TABLE fails
                                "cannot_clear_result_twice_statement", # CREATE TABLE fails
                                "get_query_n_more_rows",
                                "get_query_n_zero_rows",
                                "send_statement_trivial",
                                "send_statement_syntax_error",
                                "send_statement_result_valid",
                                "send_statement_only_one_result_set",
                                "send_statement_immediate",
                                "execute_atomic",
                                "execute_immediate",
                                "data_type_create_table",
                                "data_numeric", # TODO: numeric, logical returning as character?
                                "data_logical",
                                "data_character",
                                "data_raw",
                                "data_timestamp", # SQL statement fails
                                "data_date_typed",
                                "data_date_current_typed",
                                "data_timestamp_typed",
                                "data_timestamp_current_typed",
                                "data_64_bit_numeric_warning", # bigint handling/warning not supported yet
                                "data_64_bit_lossless"
)))
})

