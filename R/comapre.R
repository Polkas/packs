#' Compare DESCRIPTION files dependencies between specific CRAN packages versions
#' @description using the remote github CRAN mirror to compare DESCRIPTION files dependencies between specific packages versions.
#' @inheritParams standard_args
#' @param repos `character` vector repositories URLs to use. Used only for the validation. Default `https://cran.rstudio.com/`
#' @return `data.frame` with 4 columns.
#' \describe{
#' \item{Package}{character package names.}
#' \item{Version.OLD}{character versions of dependencies required by an old package version.}
#' \item{Version.NEW}{character versions of dependencies required by a new package version.}
#' \item{version_status}{ numeric -1/0/1 which comes from `utils::compareVersion` function.
#' 0 means that both versions have the same requirement. -1 means that the new version remove this requirement. 1 means that the new version added a new requirement.}
#' }
#' @export
#' @examples
#' \dontrun{
#' pacs::pac_compare_versions("memoise", "0.2.1", "2.0.0")
#' pacs::pac_compare_versions("memoise", "0.2.1")
#' # local version to newest one
#' pacs::pac_compare_versions("memoise")
#' }
pac_compare_versions <- function(pac,
                                 old = NULL,
                                 new = NULL,
                                 fields = c("Imports", "Depends", "LinkingTo"),
                                 lib.loc = .libPaths(),
                                 repos = "https://cran.rstudio.com/") {
  fields <- expand_dependency(fields)
  validate_compare_input(pac, old, new, lib.loc, repos)
  stopifnot(is_online())


  if (isFALSE(pac_isin(pac, repos))) {
    message(
      sprintf(
        "%s package is not in provided repositories %s.\n",
        pac,
        paste(repos, collapse = ", ")
      )
    )
    return(NA)
  }

  if (is.null(old)) {
    stopifnot(pac %in% rownames(installed_packages(lib.loc = lib.loc)))
    old <- pac_description(pac, local = TRUE)$Version
  }

  if (is.null(new)) {
    new <- pac_last(pac)
  }

  stopifnot(utils::compareVersion(new, old) >= 0)

  one_desc <- pac_description(pac, version = old, lib.loc = lib.loc, repos = repos)
  if (length(one_desc) == 0 || isNA(one_desc)) stop(sprintf("Version %s is not exists for %s.", old, pac))
  one_base <- paste(Filter(function(x) length(x) > 0, one_desc[fields]), collapse = ",")
  one_e <- extract_deps(one_base)
  s_remote <- unique(data.frame(
    Package = one_e$packages[[1]], Version = replaceNA(one_e$versions[[1]], ""),
    stringsAsFactors = FALSE
  ))

  two_desc <- pac_description(pac, version = new, lib.loc = lib.loc, repos = repos)
  if (length(two_desc) == 0 || isNA(two_desc)) stop(sprintf("Version %s is not exists for %s.", new, pac))
  two_base <- paste(Filter(function(x) length(x) > 0, two_desc[fields]), collapse = ",")
  two_e <- extract_deps(two_base)
  s_remote2 <- unique(data.frame(
    Package = two_e$packages[[1]], Version = replaceNA(two_e$versions[[1]], ""),
    stringsAsFactors = FALSE
  ))

  res <- merge(s_remote, s_remote2, by = c("Package"), all = TRUE, suffix = paste0(".", c(old, new)))
  col_old <- paste0("Version.", old)
  col_new <- paste0("Version.", new)
  res$version_status <- apply(res, 1, function(x) utils::compareVersion(x[col_new], x[col_old]))
  rownames(res) <- NULL
  attr(res, "package") <- pac
  attr(res, "old") <- old
  attr(res, "new") <- new
  res
}

#' Compare NAMESPACE exports between specific CRAN packages versions
#' @description using the remote github CRAN mirror to compare NAMESPACE exports between specific packages versions.
#' @inheritParams standard_args
#' @param repos `character` vector repositories URLs to use. Used only for the validation. Default `https://cran.rstudio.com/`
#' @return `list` with `c("imports", "exports", "exportPatterns", "importClasses", "importMethods", "exportClasses", "exportMethods", "exportClassPatterns", "dynlibs", "S3methods")` slots, and added and removed ones for each of them.
#' @export
#' @examples
#' \dontrun{
#' pacs::pac_compare_namespace("shiny", "1.0.0", "1.6.0")
#' pacs::pac_compare_namespace("shiny", "1.0.0", "1.6.0")$exports
#' # local version to newest one
#' pacs::pac_compare_namespace("shiny")
#' }
pac_compare_namespace <- function(pac,
                                  old = NULL,
                                  new = NULL,
                                  lib.loc = .libPaths(),
                                  repos = "https://cran.rstudio.com/") {
  validate_compare_input(pac, old, new, lib.loc, repos)
  stopifnot(is_online())


  if (isFALSE(pac_isin(pac, repos))) {
    message(
      sprintf(
        "%s package is not in provided repositories %s.\n",
        pac,
        paste(repos, collapse = ", ")
      )
    )
    return(NA)
  }

  if (is.null(old)) {
    stopifnot(pac %in% rownames(installed_packages(lib.loc = lib.loc)))
    old <- pac_description(pac, local = TRUE)$Version
  }

  if (is.null(new)) {
    new <- pac_last(pac)
  }

  stopifnot(utils::compareVersion(new, old) >= 0)

  result <- list()
  fields <- c("imports", "exports", "exportPatterns", "importClasses", "importMethods", "exportClasses", "exportMethods", "exportClassPatterns", "dynlibs", "S3methods")

  one_nam <- pac_namespace(pac, old, lib.loc = lib.loc, repos = repos)
  if (length(one_nam) == 0 || isNA(one_nam)) stop(sprintf("Version %s is not exists for %s.", old, pac))
  two_nam <- pac_namespace(pac, new, lib.loc = lib.loc, repos = repos)
  if (length(two_nam) == 0 || isNA(two_nam)) stop(sprintf("Version %s is not exists for %s.", new, pac))

  for (f in fields) {
    if (f == "S3methods") {
      old_f <- as.data.frame(one_nam[[f]])
      old_f$id <- seq_len(nrow(old_f))
      new_f <- as.data.frame(two_nam[[f]])
      new_f$id <- seq_len(nrow(new_f))

      merged <- merge(old_f, new_f, by = c("V1", "V2", "V3", "V4"), all = TRUE)
      added <- merged[is.na(merged$id.x) & !is.na(merged$id.y), 1:4]
      rownames(added) <- NULL
      removed <- merged[!is.na(merged$id.x) & is.na(merged$id.y), 1:4]
      rownames(removed) <- NULL

      result[[f]] <- list(removed = removed, added = added)
    } else {
      old_f <- unlist(one_nam[[f]])
      new_f <- unlist(two_nam[[f]])

      result[[f]] <- list(removed = setdiff(old_f, new_f), added = setdiff(new_f, old_f))
    }
  }

  structure(result, package = pac, old = old, new = new)
}

#' Compare NEWS files between specific CRAN packages versions
#' @description using the remote github CRAN mirror to compare NEWS files between specific packages versions.
#' @inheritParams standard_args
#' @param repos `character` vector repositories URLs to use. Used only for the validation. Default `https://cran.rstudio.com/`
#' @return `character` with NEWS content between specific versions.
#' @export
#' @examples
#' \dontrun{
#' pacs::pac_compare_news("shiny", "1.0.0", "1.6.0")
#' # local version to newest one
#' pacs::pac_compare_news("shiny")
#' }
pac_compare_news <- function(pac,
                             old = NULL,
                             new = NULL,
                             lib.loc = .libPaths(),
                             repos = "https://cran.rstudio.com/") {


  validate_compare_input(pac, old, new, lib.loc, repos)
  stopifnot(is_online())

  if (isFALSE(pac_isin(pac, repos))) {
    return(NA)
  }

  if (is.null(old)) {
    stopifnot(pac %in% rownames(installed_packages(lib.loc = lib.loc)))
    old <- pac_description(pac, local = TRUE)$Version
  }

  if (is.null(new)) {
    new <- pac_last(pac)
  }

  stopifnot(utils::compareVersion(new, old) >= 0)

  if (utils::compareVersion(new, old) == 0) {
    return(NA)
  }

  version_pattern <- function(version) {
      paste0("#.*", version)
  }

  last_version <- pac_last(pac, repos = repos)
  pac_news <- pac_news(pac, last_version, lib.loc = lib.loc, repos = repos)


  old_version_reg <- regexpr(version_pattern(old), pac_news)
  which_matched_old <- which(old_version_reg > 0)[1]
  old_version_pos <- if (isTRUE(which_matched_old > 0)) {
    which_matched_old
  } else {
    NA
  }

  new_version_reg <- regexpr(version_pattern(new), pac_news)
  which_matched_new <- which(new_version_reg > 0)[1]
  new_version_pos <- if (isTRUE(which_matched_new > 0)) {
    which_matched_new
  } else {
    NA
  }

  if (is.na(old_version_pos) || is.na(new_version_pos)) {
    return(NA)
  }

  result <- pac_news[new_version_pos:max(c(old_version_pos - 1, 1))]
  result
}


