# Extracted from test-chrome.R:43

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "safeconduct", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
js <- bf_focus_script(".card", 2)
expect_match(js, '})(".card", 2)', fixed = TRUE)
expect_match(js, "setProperty(prop, value, 'important')", fixed = TRUE)
expect_match(bf_focus_script(), '(".card", 1.5)', fixed = TRUE)
withr::local_options(safeconduct.focus = FALSE)
expect_null(bf_focus_script())
withr::local_options(safeconduct.focus = "#challenge-stage")
expect_match(bf_focus_script(), 'querySelector("#challenge-stage")', fixed = TRUE)
