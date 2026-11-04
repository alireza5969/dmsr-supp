# Download the supplementary files of one PMC article from Europe PMC.
# Requires: httr2.   Defines functions only (no side effects, does not source anything).
#
# download_supp() returns an integer status:
#   200  downloaded and unpacked
#   404  (or another 4xx) Europe PMC has nothing for this article -> final, not retried
#   -1   Europe PMC answered 200 but the zip could not be unpacked -> final
#   NA   transient failure (network, timeout) -> retried on the next run
#   429 / 5xx are also retried on the next run (see R/04-main_download.r)

.pad_maker <- function(num, max_num) {
  formatC(as.integer(num), width = nchar(as.character(max_num)), flag = "0")
}


# e.g. n = 137, divideBy = 100  ->  "00101-00200"
.dir_names_distribute_workload <- function(num, max_num = 24000, divideBy = 100) {
  num <- as.integer(num)
  quotient <- (num - 1L) %/% divideBy
  
  beg <- .pad_maker((divideBy * quotient) + 1L, max_num)
  end <- .pad_maker(divideBy * (quotient + 1L), max_num)
  
  paste0(beg, "-", end)
}


# Remove everything in `outdir` that is not one of the files listed for this article.
# Safety net: if NONE of the downloaded names match, keep everything
# (name mismatch, e.g. URL-encoding) instead of deleting the whole download.
.cleanup_unexpected <- function(outdir, expected_files) {
  expected <- basename(expected_files)
  found <- list.files(outdir, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  if (!length(found) || !length(expected)) return(invisible())
  
  keep <- basename(found) %in% expected
  if (!any(keep)) {
    message("None of the expected files found in ", outdir, "; keeping everything.")
    return(invisible())
  }
  file.remove(found[!keep])
  
  # drop sub-directories that became empty (children first)
  dirs <- list.dirs(outdir, recursive = TRUE, full.names = TRUE)
  for (d in rev(dirs[dirs != outdir])) {
    if (!length(list.files(d, all.files = TRUE, no.. = TRUE))) unlink(d, recursive = TRUE)
  }
  invisible()
}


download_supp <- function(n, pmcid, expected_files, out_root = "data", sleep = 0.34) {
  
  n <- as.character(n)
  outdir <- file.path(
    out_root,
    .dir_names_distribute_workload(n, divideBy = 500),
    .dir_names_distribute_workload(n, divideBy = 100),
    paste0(n, ".", pmcid)
  )
  
  url <- sprintf(
    "https://www.ebi.ac.uk/europepmc/webservices/rest/%s/supplementaryFiles",
    pmcid
  )
  
  zip_path <- tempfile(fileext = ".zip")
  on.exit(unlink(zip_path), add = TRUE)
  
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_timeout(600) |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_retry(max_tries = 3, retry_on_failure = TRUE) |>
      httr2::req_perform(path = zip_path),
    error = function(e) {
      message("Download failed for ", pmcid, ": ", conditionMessage(e))
      NULL
    }
  )
  
  Sys.sleep(sleep)
  
  if (is.null(resp)) return(NA_integer_)
  
  status <- httr2::resp_status(resp)
  if (status != 200L) return(as.integer(status))   # no directory is created for these
  
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  
  unpacked <- tryCatch(
    {
      utils::unzip(zip_path, exdir = outdir)
      TRUE
    },
    warning = function(w) FALSE,
    error   = function(e) FALSE
  )
  if (!unpacked) {
    message("Could not unzip the download for ", pmcid)
    unlink(outdir, recursive = TRUE)
    return(-1L)
  }
  
  .cleanup_unexpected(outdir, expected_files)
  
  200L
}
