# Crawl the supplementary-file list of every PMCID in paths.rds.
# Requires (already sourced): R/00-utils.r, R/00-fetch_add.r.   Defines a function only.
#
# * htmls keeps ONE ROW PER PMCID, so row indices never shift.
# * Per-file rows live in `adds`.
# * A PMCID is flagged `crawled` only if its request succeeded, so failures are retried.
# * Resumable: re-running only touches PMCIDs that are not yet crawled.

crawl_adds <- function(paths_file = "paths.rds",
                       checkpoint_every = 200L,
                       sleep = 0.34,
                       deadline = NULL) {
  
  state <- read_paths(paths_file)
  todo  <- which(!state$htmls$crawled)
  
  if (!length(todo)) {
    message("Crawl already complete.")
    return(invisible(state))
  }
  message(sprintf("Crawling %d of %d PMCIDs", length(todo), nrow(state$htmls)))
  
  pending   <- list()   # tibbles fetched since the last checkpoint
  processed <- 0L
  
  save_progress <- function() {
    if (length(pending)) {
      state$adds <<- dplyr::bind_rows(c(list(state$adds), pending)) |>
        dplyr::distinct(pmcid, num, original_file, .keep_all = TRUE) |>
        dplyr::group_by(pmcid) |>
        dplyr::mutate(max_num = dplyr::n()) |>   # number of supplementary files of that PMCID
        dplyr::ungroup()
      pending <<- list()
    }
    write_paths(state, paths_file)
    git_checkpoint("crawl: supplement lists updated")
  }
  
  tryCatch({
    for (k in seq_along(todo)) {
      
      if (out_of_time(deadline)) {
        message("Time budget reached; stopping the crawl.")
        break
      }
      
      i   <- todo[k]
      res <- get_supp_adds(state$htmls$pmcid[i])
      Sys.sleep(sleep)
      
      if (!is.null(res)) {
        if (nrow(res)) pending[[length(pending) + 1L]] <- res
        state$htmls$crawled[i] <- TRUE
      }
      
      processed <- processed + 1L
      if (k %% 25 == 0) message(sprintf("%d/%d", k, length(todo)))
      if (processed %% checkpoint_every == 0L) save_progress()
    }
  }, finally = {
    if (processed > 0L) save_progress()   # also runs after errors / break
  })
  
  invisible(state)
}
