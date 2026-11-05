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


.fetch_zip <- function(url, zip_path, http11 = FALSE) {
  req <- httr2::request(url) |>
    httr2::req_timeout(600) |>
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_retry(max_tries = 3, retry_on_failure = TRUE)
  if (http11) req <- httr2::req_options(req, http_version = 2L)   # 2 = CURL_HTTP_VERSION_1_1
  httr2::req_perform(req, path = zip_path)
}


# GitHub rejects any single file over 100 MB, and one rejected file blocks every later
# push. Files above `max_file_mb` are therefore dropped and listed in `log_file`.
.drop_oversize <- function(outdir, pmcid, max_file_mb, log_file = "download_skipped.txt") {
  f <- list.files(outdir, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  if (!length(f)) return(invisible())
  mb  <- file.size(f) / 1024^2
  big <- !is.na(mb) & mb > max_file_mb
  if (!any(big)) return(invisible())
  
  cat(sprintf("%s\t%s\t%.1f MB\n", pmcid, basename(f[big]), mb[big]),
      file = log_file, append = TRUE, sep = "")
  file.remove(f[big])
  invisible()
}


# cleanup = TRUE deletes files whose names are not in `expected_files`. It is OFF by default:
# deleting is irreversible, and the names in the BioC list do not always match the names
# inside the Europe PMC zip.
download_supp <- function(n, pmcid, expected_files, out_root = "data", sleep = 0.34,
                          cleanup = FALSE, max_file_mb = 95) {
  
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
  
  # First attempt: normal (HTTP/2). If it errors, try once more over HTTP/1.1: the
  # "HTTP/2 stream ... INTERNAL_ERROR" failures from www.ebi.ac.uk can be persistent
  # for particular articles.
  resp <- tryCatch(
    .fetch_zip(url, zip_path),
    error = function(e) {
      message("Download failed for ", pmcid, "; retrying over HTTP/1.1")
      tryCatch(
        .fetch_zip(url, zip_path, http11 = TRUE),
        error = function(e2) {
          message("Download failed for ", pmcid, ": ", conditionMessage(e2))
          NULL
        }
      )
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
  
  .drop_oversize(outdir, pmcid, max_file_mb)
  if (cleanup) .cleanup_unexpected(outdir, expected_files)
  
  200L
}
