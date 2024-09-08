
#' validate pac input
#' @keywords internal
validate_pac_input <- function(pac, version, at, local, lib.loc, repos) {
  stopifnot(length(pac) == 1 && is.character(pac))
  stopifnot(is.null(at) || inherits(at, "Date"))
  stopifnot(is.null(version) || (length(version) == 1 && is.character(version)))
  stopifnot(all(c(is.null(version), is.null(at))) || xor(!is.null(version), !is.null(at)))
  stopifnot(is.null(lib.loc) || (all(lib.loc %in% .libPaths()) && (length(list.files(lib.loc)) > 0)))
  stopifnot(is.character(repos))
  stopifnot((isFALSE(local)) ||
              (isTRUE(local) && (is.null(version) || isTRUE(utils::packageDescription(pac, lib.loc = lib.loc)$Version == version))))
}

#' validate compare input
#' @keywords internal
validate_compare_input <- function(pac, old, new, lib.loc, repos) {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(is.null(old) || (length(old) == 1) && is.character(old))
  stopifnot(is.null(new) || (length(new) == 1) && is.character(new))
  stopifnot(is.character(repos))
  stopifnot(is.null(lib.loc) || (all(lib.loc %in% .libPaths()) && (length(list.files(lib.loc)) > 0)))
}
