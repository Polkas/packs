test_that("test valid validate_pac_input input", {
    expect_silent(validate_pac_input("memoise", version = NULL, at = NULL, local = TRUE, lib.loc = .libPaths(), repos = "https://cran.rstudio.com/"))
    expect_silent(validate_pac_input("memoise", version = "1.1.0", at = NULL, local = FALSE, lib.loc = .libPaths(), repos = "https://cran.rstudio.com/"))
})

test_that("test invalid validate_pac_input input", {
    expect_error(validate_pac_input("memoise", version = "1.1.0", at = NULL, local = TRUE, lib.loc = .libPaths(), repos = "https://cran.rstudio.com/"))
    expect_error(validate_pac_input("memoise", version = "1.1.0", at = as.Date("2019-02-01"), local = FALSE, lib.loc = .libPaths(), repos = "https://cran.rstudio.com/"))
})

test_that("test valid validate_compare_input input", {
    expect_silent(validate_compare_input("memoise", old = NULL, new = NULL, lib.loc = .libPaths(), repos = "https://cran.rstudio.com/"))
    expect_silent(validate_compare_input("memoise", old = "1.1.0", new = "1.1.1", lib.loc = .libPaths(), repos = "https://cran.rstudio.com/"))
})

test_that("test invalid validate_compare_input input", {
    expect_error(validate_compare_input(c("memoise", "sth"), old = "1.1.0", new = "1.1.1", lib.loc = .libPaths(), repos = "https://cran.rstudio.com/"))
    expect_error(validate_compare_input("memoise", old = c("1.1.0", "1"), new = "1.1.1", lib.loc = .libPaths(), repos = "https://cran.rstudio.com/"))
    expect_error(validate_compare_input("memoise", old = "1.1.0", new = "1.1.1", lib.loc = .libPaths(), repos = 2))
})
