test_that("connections to internet resources fail gracefully", {
  suppressMessages(expect_message(fail_gracefully("http://httpbin.org/status/404"), "Not Found \\(HTTP 404\\)"))
  suppressMessages(expect_message(fail_gracefully("http://httpbin.org/delay/2", 1), "Timeout was reached"))
  expect_silent(fail_gracefully("http://httpbin.org/"))
})
