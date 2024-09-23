
test_that("pacs:::read_github_file valid input", {
  skip_if_offline()
  expect_silent(pacs:::read_github_file("memoise", "1.1.0", "DESCRIPTION", repos = "https://cran.rstudio.com/"))
  expect_silent(pacs:::read_github_file("memoise", "1.1.0", "NAMESPACE", repos = "https://cran.rstudio.com/"))
  expect_silent(pacs:::read_github_file("memoise", "1.1.0", "NEWS"))
})

test_that("pacs:::read_github_file invalid input", {
  skip_if_offline()
  expect_identical(pacs:::read_github_file("dplyr", "0.8.0.2", "DESCRIPTION", repos = "https://cran.rstudio.com/"),NA)
  expect_error(pacs:::read_github_file("dplyr2", "0.8.0", "NAMESPACE", repos = "https://cran.rstudio.com/"), NA)
})
