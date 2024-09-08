#' Read a file from a GitHub CRAN repository
#' @description Read a file from a GitHub CRAN repository.
#' @param pac `character` package name.
#' @param version `character` package version.
#' @param file `character` file name to read. Possible values are `DESCRIPTION` and `NAMESPACE`.
#' @param repos `character` vector repositories URLs to use. Used only for the validation. Default `https://cran.rstudio.com/`
#' @note if the file is not found in the GitHub repository, it will try to find it in the CRAN archive.
#' @keywords internal
read_github_file <- function(pac, version, file, repos = "https://cran.rstudio.com/") {
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
    result <- cran_archive_file(pac, version, repos, file)
  } else {
    res <- readLines(ee)
    unlink(ee)
  }
}
