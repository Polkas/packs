test_that("valid input pacs::pac_news", {
  skip_if_offline()
  expect_true(is.character(pac_news("dplyr", version = "0.8.0")))
  expect_true(is.character(pac_news("dplyr", at = as.Date("2019-02-01"))))
})

test_that("invalid input pacs::pac_news", {
  skip_if_offline()
  expect_identical(pac_news("dplyr", version = "0.0.0.1"), NA)
  expect_identical(pac_news("dplyr", version = "10.8.0.0"), NA)
  expect_identical(pac_news("dplyr2", at = as.Date("2019-02-01")), NA)
})
