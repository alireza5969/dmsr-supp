# Shared helpers (state file, git checkpoints, time budget).
# Sourced first by the entry-point scripts 04 and 05. No side effects.
#
# paths.rds layout:
#   htmls : one row per PMCID  -> n, pmcid, crawled, success_download
#   adds  : one row per supplementary file -> pmcid, num, original_file, max_num
#   cache : number of PMCIDs crawled so far (informational only)


empty_adds <- function() {
  tibble::tibble(
    pmcid         = character(),
    num           = integer(),
    original_file = character(),
    max_num       = integer()
  )
}


read_paths <- function(path = "paths.rds") {
  p <- readr::read_rds(path)
  
  htmls <- dplyr::ungroup(tibble::as_tibble(p$htmls))
  
  if ("num" %in% names(htmls)) {
    stop(
      "paths.rds is in the old 'expanded' format (htmls has a `num` column). ",
      "Restore the original two-column version (n, pmcid) from git history."
    )
  }
  
  if (!"crawled" %in% names(htmls)) htmls$crawled <- FALSE
  if (!"success_download" %in% names(htmls)) htmls$success_download <- NA_integer_
  htmls$crawled          <- as.logical(htmls$crawled)
  htmls$success_download <- as.integer(htmls$success_download)
  
  adds <- p$adds
  adds <- if (is.null(adds)) empty_adds() else dplyr::ungroup(tibble::as_tibble(adds))
  
  list(htmls = htmls, adds = adds)
}


write_paths <- function(state, path = "paths.rds") {
  out <- list(
    htmls = state$htmls,
    adds  = state$adds,
    cache = sum(state$htmls$crawled)
  )
  tmp <- paste0(path, ".tmp")
  readr::write_rds(out, tmp, compress = "gz")
  file.rename(tmp, path)   # atomic swap: never leave a half-written paths.rds
  invisible(path)
}


# Stage everything, commit if there is anything to commit, push.
# A failed push is only a warning: the workflow's final step retries it.
# Set NO_GIT=1 to disable (handy for local testing).
git_checkpoint <- function(msg, push = TRUE) {
  if (nzchar(Sys.getenv("NO_GIT"))) return(invisible(FALSE))
  
  gert::git_add(files = ".")
  if (nrow(gert::git_status(staged = TRUE)) == 0) return(invisible(FALSE))
  
  gert::git_commit(msg)
  if (push) {
    tryCatch(
      gert::git_push(),
      error = function(e) message("git push failed (will be retried later): ", conditionMessage(e))
    )
  }
  invisible(TRUE)
}


# Optional wall-clock budget, e.g. TIME_BUDGET_MIN=315 on GitHub Actions.
# Returns NULL (= no limit) when the variable is unset.
make_deadline <- function(minutes = Sys.getenv("TIME_BUDGET_MIN", "")) {
  m <- suppressWarnings(as.numeric(minutes))
  if (is.na(m) || !is.finite(m)) return(NULL)
  Sys.time() + m * 60
}

out_of_time <- function(deadline) {
  !is.null(deadline) && Sys.time() > deadline
}
