read_version_info <- function(base_dir = ".", fallback_version = "dev", fallback_date = as.character(Sys.Date())) {
  # Find VERSION.md by searching up from base_dir
  normalize <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  base_dir <- normalize(base_dir)
  if (!nzchar(base_dir)) base_dir <- normalize(getwd())

  # Ascend up to 6 levels looking for VERSION.md
  cur <- base_dir
  version_path <- NULL
  for (i in 1:6) {
    cand <- file.path(cur, "VERSION.md")
    if (file.exists(cand)) { version_path <- cand; break }
    parent <- normalize(dirname(cur))
    if (identical(parent, cur)) break
    cur <- parent
  }

  ver <- fallback_version
  dt  <- fallback_date

  if (!is.null(version_path)) {
    lines <- tryCatch(readLines(version_path, warn = FALSE), error = function(e) character())
    if (length(lines)) {
      # Parse Current Version (support bold markdown or plain text)
      idx <- grep("Current Version:", lines, fixed = TRUE)
      if (length(idx)) {
        vv <- sub(".*Current Version:\\s*", "", lines[idx[1]])
        vv <- gsub("\\*", "", vv)
        vv <- trimws(vv)
        if (grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", vv)) ver <- vv
      }
      # Parse Last Updated
      idx2 <- grep("Last Updated:", lines, fixed = TRUE)
      if (length(idx2)) {
        dd <- sub(".*Last Updated:\\s*", "", lines[idx2[1]])
        dd <- gsub("\\*", "", dd)
        dd <- trimws(dd)
        if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", dd)) dt <- dd
      }
    }
  }

  # Short Git SHA and tag
  git <- Sys.which("git")
  sha <- "unknown"
  tag <- ""
  if (nzchar(git)) {
    sha_out <- tryCatch(system2(git, c("rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE), error = function(e) character())
    sha_out <- sha_out[nzchar(sha_out)]
    if (length(sha_out)) sha <- sha_out[1]

    # Prefer a tag that points at HEAD; fallback to the latest reachable tag
    tag_out <- tryCatch(system2(git, c("tag", "--points-at", "HEAD"), stdout = TRUE, stderr = FALSE), error = function(e) character())
    tag_out <- tag_out[nzchar(tag_out)]
    if (length(tag_out)) {
      tag <- tag_out[1]
    } else {
      desc_out <- tryCatch(system2(git, c("describe", "--tags", "--abbrev=0"), stdout = TRUE, stderr = FALSE), error = function(e) character())
      desc_out <- desc_out[nzchar(desc_out)]
      if (length(desc_out)) tag <- desc_out[1]
    }
  }

  # Environment fallbacks (useful when git/VERSION.md unavailable inside Shiny)
  ver_env <- Sys.getenv("CVALR_APP_VERSION", "")
  if (nzchar(ver_env)) ver <- ver_env
  sha_env <- Sys.getenv("CVALR_APP_BUILD_SHA", "")
  if (nzchar(sha_env)) sha <- sha_env
  tag_env <- Sys.getenv("CVALR_APP_BUILD_TAG", "")
  if (nzchar(tag_env)) tag <- tag_env
  date_env <- Sys.getenv("CVALR_APP_VERSION_DATE", "")
  if (nzchar(date_env)) dt <- date_env

  list(
    version = ver,
    date = dt,
    sha = sha,
    tag = tag,
    build_label = sprintf("v%s (%s%s)", ver, sha, if (nzchar(tag)) paste0(", ", tag) else "")
  )
}

