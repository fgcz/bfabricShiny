context("app_logging")

test_that("setUpAppLogging requires session object", {
  expect_error(
    bfabricShiny:::setUpAppLogging(NULL, NULL),
    "invalid first argument"
  )
})

test_that("LOG_DIR environment variable is respected", {
  # Save original value

  original_log_dir <- Sys.getenv("LOG_DIR", unset = NA)

  # Set a custom log directory

  Sys.setenv(LOG_DIR = "/tmp/test_logs")
  expect_equal(Sys.getenv("LOG_DIR"), "/tmp/test_logs")

  # Restore original value

  if (is.na(original_log_dir)) {
    Sys.unsetenv("LOG_DIR")
  } else {
    Sys.setenv(LOG_DIR = original_log_dir)
  }
})

test_that("default LOG_DIR is ./logs when unset", {
  # Save and unset

  original_log_dir <- Sys.getenv("LOG_DIR", unset = NA)
  Sys.unsetenv("LOG_DIR")

  expect_equal(Sys.getenv("LOG_DIR", unset = "./logs"), "./logs")

  # Restore

  if (!is.na(original_log_dir)) {
    Sys.setenv(LOG_DIR = original_log_dir)
  }
})
