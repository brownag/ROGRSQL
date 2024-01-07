
dbGetQuery(db2, "SELECT 10000000000")
dbSendQuery(db2, DBItest:::trivial_query()) |> dbFetch()
dbSendQuery(db2, "SELLECT", params = list())
dbSendQuery(db2, "SELLECT", immediate = TRUE)
dbSendQuery(db2, DBItest:::trivial_query())
res <- dbSendQuery(db2, DBItest:::trivial_query())
res1 <- dbSendQuery(db2, DBItest:::trivial_query())
res2 <- dbSendQuery(db2, "SELECT 2")
expect_false(dbIsValid(res1))
expect_true(dbIsValid(res2))

query <- DBItest:::trivial_query()
res <- dbSendQuery(db2, query)
dbClearResult(res)

expect_error(dbFetch(res))
