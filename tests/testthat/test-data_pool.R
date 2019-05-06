context("add_data_pool")

test_that("fails for non data.frame", {
  expect_error(add_data_pool(list(1,2,3)))
})

test_that("preserves metadata", {
  test_existing <- "test1234"
  rmarkdown::output_metadata$set(rsc_output_files = test_existing)
  
  start_metadata <- rmarkdown::output_metadata$get("rsc_output_files")
  
  tmp_preserve <- data.frame(a = c(1,2,3), b = c(4,5,6))
  add_data_pool(tmp_preserve)
  
  first_metadata <- rmarkdown::output_metadata$get("rsc_output_files")
  expect_true(test_existing %in% first_metadata)
})

test_that("writes feather by default", {
  tmp_feather <- data.frame(a = c(1,2,3), b = c(4,5,6))
  add_data_pool(tmp_feather)
  first_metadata <- rmarkdown::output_metadata$get("rsc_output_files")
  
  expect_true("tmp_feather.feather" %in% first_metadata)
  expect_true(file.exists("tmp_feather.feather"))
})

test_that("does not duplicate records", {
  start_metadata <- rmarkdown::output_metadata$get("rsc_output_files")
  
  tmp_data <- data.frame(a = c(1,2,3), b = c(4,5,6))
  add_data_pool(tmp_data)
  first_metadata <- rmarkdown::output_metadata$get("rsc_output_files")
  
  expect_true("tmp_data.feather" %in% first_metadata)
  
  add_data_pool(tmp_data)
  second_metadata <- rmarkdown::output_metadata$get("rsc_output_files")
  
  expect_identical(first_metadata, second_metadata)
})

test_that("writes json output", {
  expect_true(file.exists("connectapi_data_pool.json"))
})

test_that("json output has attributes", {
  start_metadata <- rmarkdown::output_metadata$get("rsc_output_files")
  tmp_json_attribute <- data.frame(a=c(1,2,3,4))
  res <- add_data_pool(tmp_json_attribute)
  
  expect_named(
    res$tmp_json_attribute, 
    c("filename", "size", "format", "nrow", "ncol", "colname", "coltype", "colclass")
  )
})

test_that("json output is an output file", {
  start_metadata <- rmarkdown::output_metadata$get("rsc_output_files")
  
  tmp_json_output <- data.frame(a=c(1,2,3))
  add_data_pool(tmp_json_output)
  first_metadata <- rmarkdown::output_metadata$get("rsc_output_files")
  
  expect_true("connectapi_data_pool.json" %in% first_metadata)
})

test_that("specified name is honored", {
  skip("not implemented")
})

test_that("specified filename is honored", {
  skip("not implemented")
})
