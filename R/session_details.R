#' Track session details
#'
#' Print and compare details of the current and prior sessions using [sessioninfo::session_info()] and [sessioninfo::session_diff()].
#'
#' @return A `session_info` object written to a `TXT` file ("~/session_info.txt") and a `session_diff` object saved to an `RDS` file ("~/session_diff.rds").
#' @importFrom sessioninfo session_info session_diff
#' @export
session_details <- function() {
  if (!file.exists("session_info.txt")) {
    session_info(to_file = "session_info.txt")
  }

  sess_old <- readLines("session_info.txt")
  sess_new <- session_diff(sess_old, session_info())

  idx <- grep("^ date", sess_old)

  if (any(sess_old[-idx] != sess_new$new$text[-idx])) {
    saveRDS(sess_new, file = "session_diff.rds")
    writeLines(sess_new$new$text, con = "session_info.txt")
  }
}
