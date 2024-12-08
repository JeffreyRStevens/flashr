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
