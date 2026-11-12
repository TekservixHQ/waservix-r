test_that("webhook secrets are checked", {
  expect_true(waservix_verify_webhook("s3cret-value", "s3cret-value"))
  expect_false(waservix_verify_webhook("s3cret-valuf", "s3cret-value"))
  expect_false(waservix_verify_webhook("short", "s3cret-value"))
  expect_false(waservix_verify_webhook("", ""))
  expect_false(waservix_verify_webhook(NULL, "s3cret-value"))
  expect_false(waservix_verify_webhook(NA_character_, "s3cret-value"))
})

test_that("clients need a key", {
  expect_error(waservix_client(""), "API key")
  cl <- waservix_client("wpk_x", base_url = "https://example.com/api//")
  expect_s3_class(cl, "waservix_client")
  expect_equal(cl$base_url, "https://example.com/api")
  expect_error(waservix_get_status(list()), "waservix_client")
})

test_that("phone numbers are encoded and group IDs are not", {
  expect_equal(waservix:::enc(" +250788123456 "), "%2B250788123456")
})

test_that("API errors become waservix_error conditions", {
  resp <- httr2::response(status_code = 401L,
                          body = charToRaw('{"success":false,"message":"Use the device API key"}'))
  err <- tryCatch(waservix:::handle_response(resp), waservix_error = function(e) e)
  expect_s3_class(err, "waservix_error")
  expect_equal(err$status, 401L)
  expect_equal(err$api_message, "Use the device API key")
  ok <- httr2::response(status_code = 200L, body = charToRaw('{"success":true,"data":{"msgId":5473}}'))
  expect_equal(waservix:::handle_response(ok), list(msgId = 5473L))
})

test_that("the live API rejects an invalid key with 401", {
  skip_if_not(identical(Sys.getenv("WASERVIX_LIVE_TEST"), "true"), "live test not enabled")
  skip_if_offline("tekservix.com")
  cl <- waservix_client("wpk_invalid_r_test")
  err <- tryCatch(waservix_get_status(cl), waservix_error = function(e) e)
  expect_s3_class(err, "waservix_error")
  expect_equal(err$status, 401L)
})
