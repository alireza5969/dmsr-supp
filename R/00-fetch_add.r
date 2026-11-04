# Fetch the list of supplementary files of one PMC article from NCBI BioNLP.
# Requires: httr2, jsonlite, tibble.
#
# Returns
#   * a tibble(pmcid, num, original_file)  on success; zero rows = "no supplements"
#   * NULL                                 when the request FAILED (network, 5xx, bad JSON)
# The caller must not mark a PMCID as crawled when it gets NULL, so it is retried later.

.chr1 <- function(x) if (is.null(x)) NA_character_ else as.character(x)[1]
.int1 <- function(x) if (is.null(x)) NA_integer_   else as.integer(x)[1]


.parse_adds <- function(body, pmcid) {
  empty <- tibble::tibble(pmcid = character(), num = integer(), original_file = character())
  
  body <- trimws(body)
  if (!nzchar(body)) return(empty)
  
  # OBSERVED: for an article without supplementary files the API answers HTTP 200
  # with the plain-text body "[Error] : No result can be found" (not JSON, not a 404).
  # That is a successful "nothing there", not a failure. Any other "[Error]" is a failure.
  if (grepl("^\\[Error\\]", body)) {
    if (grepl("No result can be found", body, fixed = TRUE)) return(empty)
    stop("API error: ", substr(body, 1, 200))
  }
  
  recs <- jsonlite::parse_json(body)
  if (length(recs) == 0) return(empty)
  if (!is.null(names(recs))) recs <- list(recs)   # a single record, not a list of them
  
  out <- lapply(recs, function(r) {
    tibble::tibble(
      pmcid         = pmcid,                       # we know it; no need to rebuild it from the reply
      num           = .int1(r$num),
      original_file = .chr1(r$original_file)
    )
  })
  out <- do.call(rbind, out)
  out[!is.na(out$original_file), ]
}


get_supp_adds <- function(pmcid, timeout = 10, max_tries = 3) {
  
  url <- paste0(
    "https://www.ncbi.nlm.nih.gov/research/bionlp/RESTful/supplmat.cgi/BioC_JSON/",
    pmcid, "/list"
  )
  
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_timeout(timeout) |>
      httr2::req_retry(max_tries = max_tries, retry_on_failure = TRUE) |>
      httr2::req_error(is_error = \(r) FALSE) |>   # inspect the status ourselves
      httr2::req_perform(),
    error = function(e) {
      message("Failed for ", pmcid, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(resp)) return(NULL)
  
  status <- httr2::resp_status(resp)
  
  # A 404 is also treated as "no supplementary files". (In practice the API answers
  # 200 + "[Error] : No result can be found" for that case; see .parse_adds().)
  if (status == 404L) {
    return(tibble::tibble(pmcid = character(), num = integer(), original_file = character()))
  }
  if (status != 200L) {
    message("Failed for ", pmcid, ": HTTP ", status)
    return(NULL)
  }
  
  tryCatch(
    .parse_adds(httr2::resp_body_string(resp), pmcid),
    error = function(e) {
      message("Could not parse reply for ", pmcid, ": ", conditionMessage(e))
      NULL
    }
  )
}
