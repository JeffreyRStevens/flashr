#' Checks if a character input is valid
#'
#' @param name Argument name.
#' @param value Argument value.
#' @param allowed Allowed argument values.
#' @param nullok Logical for whether NULL values are OK (TRUE) or not (FALSE).
#'
#' @keywords internal
#'
check_character <- function(name = NULL,
                            value = NULL,
                            allowed = NULL,
                            nullok = FALSE) {
  if (is.null(name)) cli::cli_abort(paste0("Enter valid `name`."))
  if (!nullok) {
    if (is.null(value)) cli::cli_abort(paste0("Enter valid `value`."))
    if (!is.character(value)) {
      cli::cli_abort(
        "`{name}` must be a character string."
      )
    }
  } else {
    if (!is.character(value) && !is.null(value)) cli::cli_abort("`{name}` must be a character string or NULL.")
  }
  if (!is.null(allowed)) {
    if (!value %in% allowed) cli::cli_abort("`{name}` must be one of the allowed values: {allowed}.")
  }
}


#' Checks if a logical input is valid
#'
#' @param name Argument name.
#' @param value Argument value.
#'
#' @keywords internal
#'
check_logical <- function(name = NULL, value = NULL) {
  if (is.null(name)) cli::cli_abort(paste0("Enter valid `name`."))
  if (is.null(value)) cli::cli_abort(paste0("Enter valid `value`."))
  if (!is.logical(value)) cli::cli_abort("`{name}` must be a logical (TRUE or FALSE).")
}


#' Gracefully fail if internet connection is not available
#'
#' CRAN policies require that "Packages which use Internet resources should fail
#' gracefully with an informative message if the resource is not available or
#' has changed (and not give a check warning nor error)." This solution is
#' adapted from kvasilopoulos' response at
#' <https://forum.posit.co/t/internet-resources-should-fail-gracefully/49199/11>.
#' \emph{This function is not exported.}
#'
#' @param remote_file Remote file to be downloaded.
#' @param maxtime Maximum time to check connection before timing out.
#'
#' @keywords internal
#'
fail_gracefully <- function(remote_file, maxtime = 10) {
  try_GET <- function(x, ...) {
    tryCatch(
      httr::GET(url = x, httr::timeout(maxtime), ...),
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }
  is_response <- function(x) {
    class(x) == "response"
  }

  # First check internet connection
  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }
  # Then try for timeout problems
  resp <- try_GET(remote_file)
  if (!is_response(resp)) {
    message(resp)
    return(invisible(NULL))
  }
  # Then stop if status > 400
  if (httr::http_error(resp)) {
    httr::message_for_status(resp)
    return(invisible(NULL))
  }
}
