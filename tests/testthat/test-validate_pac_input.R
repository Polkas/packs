test_that("test valid validate_pac_input input", {
    expect_silent(validate_pac_input("memoise", version = NULL, at = NULL, local = TRUE, lib.loc = .libPaths(), repos = "https://cran.rstudio.com/"))
    expect_silent(validate_pac_input("memoise", version = "1.1.0", at = NULL, local = FALSE, lib.loc = .libPaths(), repos = "https://cran.rstudio.com/"))
})

test_that("test invalid validate_pac_input input", {
    expect_error(validate_pac_input("memoise", version = "1.1.0", at = NULL, local = TRUE, lib.loc = .libPaths(), repos = "https://cran.rstudio.com/"))
    expect_error(validate_pac_input("memoise", version = "1.1.0", at = as.Date("2019-02-01"), local = FALSE, lib.loc = .libPaths(), repos = "https://cran.rstudio.com/"))
})
