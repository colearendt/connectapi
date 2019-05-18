
# should connect with env vars
test_conn_1 <- Connect$new(host = Sys.getenv("TEST_SERVER_1"), api_key = Sys.getenv("TEST_KEY_1"))
test_conn_2 <- Connect$new(host = Sys.getenv("TEST_SERVER_2"), api_key = Sys.getenv("TEST_KEY_2"))

cont1_name <- uuid::UUIDgenerate()
cont1_title <- "Data Pool 1"
cont1_guid <- NULL
cont1_bundle <- NULL
cont1_content <- NULL

context("add_data_pool")

test_that("fails on non-data.frame", {
  skip("not implemented")
})
test_that("fails for bad file names", {
  skip("not implemented")
})
test_that("handles disallowed characters", {
  skip("not implemented")
})

context("data_pool")



context("list_data_pools")

test_that("ensures content uniqueness", {
  skip("not implemented")
})

test_that("collapse_by_id handles edge cases properly", {
  skip("not implemented")
})

