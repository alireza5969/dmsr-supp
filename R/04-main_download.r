# Entry point: (1) crawl the supplement lists, (2) download the supplements.
# Run from the project root:   Rscript R/04-main_download.r
# Optional env vars: TIME_BUDGET_MIN (stop cleanly after N minutes), NO_GIT=1 (no commits).
# Requires: readr, dplyr, tibble, httr2, jsonlite (and the git command line).

source("R/00-utils.r")
source("R/00-fetch_add.r")
source("R/01-accumulate_adds.r")
source("R/02-download_supps.r")

deadline <- make_deadline()

# 1. Crawl (a no-op once every PMCID is crawled) ------------------------------
crawl_adds(deadline = deadline)

# 2. Download -----------------------------------------------------------------
state <- read_paths()
htmls <- state$htmls
adds  <- state$adds

# 200 / 404 / -1 are final; NA, 429 and 5xx are retried
needs_retry <- function(x) is.na(x) | x == 429L | x >= 500L

# Row numbers of the articles still to download. Everything else is dropped HERE,
# once, so the loop below never even sees it.
todo <- which(
  htmls$crawled &
    htmls$pmcid %in% adds$pmcid &          # only articles that actually have supplements
    needs_retry(htmls$success_download)
)

expected <- split(adds$original_file, adds$pmcid)

message(sprintf("%d articles to download", length(todo)))

processed <- 0L

save_progress <- function() {
  write_paths(list(htmls = htmls, adds = adds))
  git_checkpoint("download: files added")
}

tryCatch({
  for (i in todo) {                       # i is a row number of htmls
    
    if (out_of_time(deadline)) {
      message("Time budget reached; stopping the download.")
      break
    }
    
    curr_pmcid <- htmls$pmcid[i]
    
    res <- download_supp(htmls$n[i], curr_pmcid, expected[[curr_pmcid]])
    
    htmls$success_download[i] <- res      # direct index: no scan of the whole table
    
    processed <- processed + 1L
    if (processed %% 20L == 0L) {
      message(sprintf("%d/%d", processed, length(todo)))
      save_progress()
    }
  }
}, finally = {
  if (processed > 0L) save_progress()
})

print(table(htmls$success_download, useNA = "ifany"))
