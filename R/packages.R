# ============================================================
# Package management and reproducibility records
# ============================================================

#' Install (if needed) and load the packages used in the analysis
#'
#' @param packages Character vector of CRAN package names.
#' @param install_missing If TRUE, packages that are not installed are
#'   installed from CRAN. If FALSE, the function stops and lists them.
#' @return Invisibly, `packages`.
load_packages <- function(packages, install_missing = TRUE) {
  # Identify packages that are not currently installed
  missing_packages <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) > 0) {
    if (!install_missing) {
      stop("Missing packages: ", paste(missing_packages, collapse = ", "),
           ". Install them or set install_missing = TRUE.", call. = FALSE)
    }
    message("Installing missing packages: ",
            paste(missing_packages, collapse = ", "))
    utils::install.packages(missing_packages)
  }

  # Load the packages without displaying their usual startup messages
  invisible(lapply(
    packages,
    function(pkg) suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
    )
  ))
  invisible(packages)
}

#' Save the R session details (R and package versions)
#'
#' @param output_dir Folder in which `session_info.txt` is written.
#' @return Invisibly, the path of the file.
save_session_info <- function(output_dir) {
  session_file <- file.path(output_dir, "session_info.txt")
  writeLines(utils::capture.output(utils::sessionInfo()), session_file)
  invisible(session_file)
}
