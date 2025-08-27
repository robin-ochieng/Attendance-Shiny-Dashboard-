# Shared helpers for download handlers across modules

#' Clean data for download
#' - Ensure data.frame/tibble
#' - Replace NA with empty string to avoid Excel showing NA
#' - Optionally reorder/rename columns if needed (future)
safe_download_df <- function(x) {
  if (is.null(x)) return(data.frame())
  # Convert tibbles to data.frame to avoid rownames in csv, etc.
  if (!is.data.frame(x)) x <- as.data.frame(x)
  # Drop rownames
  rownames(x) <- NULL
  # Coerce columns to safe types for CSV/XLSX
  x[] <- lapply(x, function(col) {
    # Handle list columns by collapsing to character
    if (is.list(col)) {
      return(vapply(col, function(v) paste0(v, collapse = "; "), character(1)))
    }
    # hms or difftime -> character HH:MM:SS
    if (inherits(col, c("hms", "difftime"))) {
      secs <- as.numeric(col)
      hh <- sprintf("%02d", floor(secs / 3600))
      mm <- sprintf("%02d", floor((secs %% 3600) / 60))
      ss <- sprintf("%02d", round(secs %% 60))
      out <- paste(hh, mm, ss, sep = ":")
      out[is.na(secs)] <- ""
      return(out)
    }
    # Factors -> character
    if (is.factor(col)) {
      col <- as.character(col)
      col[is.na(col)] <- ""
      return(col)
    }
    # Character -> replace NA with empty
    if (is.character(col)) {
      col[is.na(col)] <- ""
      return(col)
    }
    # Leave POSIXct/Date/numeric/logical as-is (writers handle them)
    col
  })
  x
}

#' Write CSV safely
write_csv_safe <- function(dat, file) {
  dat <- safe_download_df(dat)
  utils::write.csv(dat, file, row.names = FALSE, na = "")
}

#' Write XLSX safely
write_xlsx_safe <- function(dat, file) {
  dat <- safe_download_df(dat)
  writexl::write_xlsx(dat, path = file)
}
