test_that("pacs::pac_news", {
  skip_if_offline()
  expect_true(is.character(pac_news("dplyr", version = "0.8.0")))
  expect_true(is.character(pac_news("dplyr", at = as.Date("2019-02-01"))))
})