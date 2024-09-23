test_that("pacs::pac_description", {
  skip_if_offline()
  expect_true(length(pac_description("dplyr", version = "0.8.0")) == 23)
  expect_true(utils::compareVersion(
    pac_description("memoise", local = TRUE)$Version,
    pac_description("memoise", local = FALSE)$Version
  ) %in% c(0, -1))
  expect_identical(pac_description("dplyr", "10.1.1.1"), NA)
  expect_identical(pac_description("WRONG"), NA)
  expect_identical(pac_description("WRONG", local = TRUE), NA)
  expect_identical(pac_description("dplyr", "0.0.0.1"), NA)
  expect_silent(pac_description("dplyr", version = pac_last("dplyr")))
  expect_true(as.Date(pac_description("memoise", at = as.Date("2019-01-01"))[["Date/Publication"]]) <= as.Date("2019-01-01"))
})
