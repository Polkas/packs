#' Read a file from a GitHub CRAN repository
#' @description Read a file from a GitHub CRAN repository.
#' @inheritParams standard_args
#' @note if the file is not found in the GitHub repository, it will try to find it in the CRAN archive.
#' @keywords internal
read_github_file <- function(pac, version, file, repos = "https://cran.rstudio.com/") {
  stopifnot(is_online())
  ee <- tempfile()
  d_url <- sprintf(
    "https://raw.githubusercontent.com/cran/%s/%s/%s",
    pac,
    version,
    file
  )
  tt <- try(
    expr = {
      suppressWarnings(utils::download.file(d_url,
        destfile = ee,
        quiet = TRUE
      ))
    },
    silent = TRUE
  )
  if (inherits(tt, "try-error")) {
    result <- read_cran_file(pac, version, file, repos)
  } else {
    result <- readLines(ee)
    unlink(ee)
  }
  result
}
