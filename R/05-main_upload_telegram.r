# Entry point: upload downloaded supplement files to Telegram.
# Run from the project root:   Rscript R/05-main_upload_telegram.r
# Env vars: TG_TOKEN, CHAT_ID (required); TIME_BUDGET_MIN, NO_GIT=1 (optional).
# Requires: readr, dplyr, tibble, fs, httr2, curl (and the git command line).

source("R/00-utils.r")
source("R/03-upload_telegram.r")

# Settings --------------------------------------------------------------------
DATA_DIR      <- "data"
UPLOAD_GLOB   <- "*.html"             # which supplement files to send; "*" = everything
CACHE_FILE    <- "upload_cache.txt"   # one uploaded file path per line (append-only)
FAILURES_FILE <- "upload_failures.csv"  # failures/skips of the latest run
SLEEP_SECONDS <- 3.025                # stays under Telegram's 20 messages/min group limit
COMMIT_EVERY  <- 100L
MAX_CONSECUTIVE_FAILURES <- 5L        # e.g. a revoked token: stop instead of failing 20k times

deadline <- make_deadline()


allocate <- function(n) {
  dplyr::case_when(
    dplyr::between(n, 12013, 17937) ~ "mas",
    dplyr::between(n, 6165, 12012)  ~ "ajk",
    dplyr::between(n, 1, 6164)      ~ "fzm",
    dplyr::between(n, 17938, 23968) ~ "kf",
    TRUE                            ~ "unassigned"
  )
}


# What is there, what is already uploaded -------------------------------------
if (!dir.exists(DATA_DIR)) {
  message("No ", DATA_DIR, "/ directory yet; nothing to upload.")
  quit(save = "no", status = 0)
}

files <- as.character(fs::dir_ls(DATA_DIR, recurse = TRUE, type = "file", glob = UPLOAD_GLOB))

uploaded <- if (file.exists(CACHE_FILE)) readLines(CACHE_FILE) else character()

all_sups <- tibble::tibble(file = files) |>
  dplyr::mutate(
    dir    = dirname(file),
    folder = basename(dir)                 # "<n>.<pmcid>", e.g. "00001.PMC10006621"
  )

bad_layout <- !grepl("^[0-9]+\\.PMC[0-9]+$", all_sups$folder)
if (any(bad_layout)) {
  message(sum(bad_layout), " file(s) are not inside a <n>.<pmcid> folder; ignoring them.")
  all_sups <- all_sups[!bad_layout, ]
}

all_sups <- all_sups |>
  dplyr::mutate(
    n     = sub("\\..*$", "", folder),
    pmcid = sub("^[0-9]+\\.", "", folder)
  ) |>
  # sequence numbers are computed BEFORE dropping uploaded files,
  # so "2/5" stays "2/5" after a partial run
  dplyr::group_by(dir) |>
  dplyr::mutate(seq = dplyr::row_number(), seq_max = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::filter(!file %in% uploaded)

message(sprintf("%d files to upload", nrow(all_sups)))


# Upload loop -----------------------------------------------------------------
attempted <- 0L
consecutive_failures <- 0L
failures <- list()

write_failures <- function() {
  if (length(failures)) readr::write_csv(dplyr::bind_rows(failures), FAILURES_FILE)
}

save_progress <- function() {
  write_failures()
  git_checkpoint("upload: progress")
}

tryCatch({
  for (i in seq_len(nrow(all_sups))) {
    
    if (out_of_time(deadline)) {
      message("Time budget reached; stopping the upload.")
      break
    }
    
    row <- all_sups[i, ]
    
    caption <- sprintf(
      "number: #%s\nreviewer: #%s\nPMC ID: #%s\nSequence number: %d/%d",
      row$n, allocate(as.integer(row$n)), row$pmcid, row$seq, row$seq_max
    )
    
    outcome <- tryCatch({
      res <- tg_send_document(path = row$file, caption = caption)
      if (isTRUE(res$ok)) list(status = "ok", msg = NA_character_)
      else list(status = "failed", msg = "ok = FALSE")
    },
    tg_file_too_large = function(e) list(status = "skipped", msg = conditionMessage(e)),
    error             = function(e) list(status = "failed",  msg = conditionMessage(e)))
    
    if (outcome$status == "ok") {
      cat(row$file, "\n", sep = "", file = CACHE_FILE, append = TRUE)
      consecutive_failures <- 0L
    } else {
      message(outcome$status, ": ", row$file, " - ", outcome$msg)
      failures[[length(failures) + 1L]] <- tibble::tibble(
        file = row$file, status = outcome$status, error = outcome$msg,
        time = format(Sys.time())
      )
      if (outcome$status == "failed") {
        consecutive_failures <- consecutive_failures + 1L
        if (consecutive_failures >= MAX_CONSECUTIVE_FAILURES) {
          stop(MAX_CONSECUTIVE_FAILURES, " uploads failed in a row; aborting. Last error: ", outcome$msg)
        }
      }
    }
    
    attempted <- attempted + 1L
    if (attempted %% COMMIT_EVERY == 0L) save_progress()
    
    if (outcome$status != "skipped") Sys.sleep(SLEEP_SECONDS)   # skipped = no request was made
  }
}, finally = {
  save_progress()
})
