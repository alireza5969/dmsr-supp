# Send one file to a Telegram chat through the Bot API.
# Requires: httr2, curl.   Reads TG_TOKEN and CHAT_ID from the environment.
#
# Throws on failure. A file over the Bot API limit raises a condition of class
# "tg_file_too_large" (no request is made) so callers can skip it instead of aborting.

TG_MAX_BYTES <- 50 * 1024^2   # Bot API limit for sendDocument


tg_send_document <- function(
    token   = Sys.getenv("TG_TOKEN"),
    chat_id = Sys.getenv("CHAT_ID"),
    path,
    caption = NULL,
    max_tries = 5
) {
  stopifnot(file.exists(path), nzchar(token), nzchar(chat_id))
  
  size <- file.size(path)
  if (size > TG_MAX_BYTES) {
    stop(structure(
      class = c("tg_file_too_large", "error", "condition"),
      list(
        message = sprintf("%s is %.1f MB; Bot API limit is 50 MB", path, size / 1024^2),
        call = NULL
      )
    ))
  }
  
  # Scale the total timeout with file size: at least 2 min, ~1 min per 5 MB
  size_mb <- size / 1024^2
  total_timeout <- max(120, ceiling(size_mb / 5) * 60)
  
  fields <- list(chat_id = chat_id, document = curl::form_file(path))
  if (!is.null(caption)) fields$caption <- as.character(caption)
  
  req <- httr2::request(sprintf("https://api.telegram.org/bot%s/sendDocument", token))
  req <- do.call(httr2::req_body_multipart, c(list(req), fields))
  
  req <- req |>
    httr2::req_timeout(total_timeout) |>
    httr2::req_options(
      connecttimeout  = 15,     # fail fast if Telegram is unreachable
      low_speed_limit = 1000,   # abort if throughput stays under 1 KB/s ...
      low_speed_time  = 60      # ... for 60 s (stalled upload)
    ) |>
    httr2::req_retry(
      max_tries        = max_tries,
      retry_on_failure = TRUE,  # also retry network errors and timeouts
      is_transient     = \(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504),
      # honour Telegram's flood-control hint on 429s
      after = \(resp) {
        if (httr2::resp_status(resp) == 429) {
          ra <- tryCatch(httr2::resp_body_json(resp)$parameters$retry_after,
                         error = \(e) NULL)
          if (!is.null(ra)) return(ra + 1)
        }
        NA  # otherwise fall back to exponential backoff
      },
      backoff = \(i) min(60, 2^i)  # 2, 4, 8, 16, 32 s ...
    ) |>
    httr2::req_error(body = \(resp) {
      # surface Telegram's own error text (e.g. "Bad Request: chat not found")
      tryCatch(httr2::resp_body_json(resp)$description, error = \(e) NULL)
    })
  
  resp <- httr2::req_perform(req)
  out  <- httr2::resp_body_json(resp)
  if (!isTRUE(out$ok)) stop("Telegram returned ok = FALSE: ", out$description)
  out
}
