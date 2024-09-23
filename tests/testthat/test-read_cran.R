test_that("pacs:::read_cran_file valid input", {
  skip_if_offline()
  expect_silent(pacs:::read_cran_file("memoise", "1.1.0", "DESCRIPTION", repos = "https://cran.rstudio.com/"))
  expect_silent(pacs:::read_cran_file("memoise", "1.1.0", "NAMESPACE", repos = "https://cran.rstudio.com/"))
  expect_silent(pacs:::read_cran_file("memoise", "1.1.0", "NEWS"))
})

test_that("pacs::read_cran_file expected output", {
  skip_if_offline()
  expect_true(length(read_cran_file("dplyr", "1.0.0", "DESCRIPTION", "https://cran.rstudio.com/")) == 22)
  expect_identical(
    read_cran_file("dplyr", "0.0.0.1", "DESCRIPTION", "https://cran.rstudio.com/"),
    NA
  )
})
