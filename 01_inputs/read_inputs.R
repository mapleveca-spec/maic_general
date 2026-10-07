# Reading inputs from flat files -----------------------------------------------
#
# Analysts maintain the metadata in a spreadsheet or CSV. The only column that
# does not survive a flat file is `level_order`, which is a list column in R.
# In the file it is one string per row with levels separated by `sep`
# ("Mild|Moderate|Severe"), empty for continuous variables.
#
# metadata_from_table()  converts such a data.frame into a valid metadata
#                        table: splits level_order, coerces the flag columns
#                        to logical (accepting TRUE/FALSE, T/F, 1/0, yes/no),
#                        and validates.
# read_metadata_csv()    read.csv() followed by metadata_from_table().
#
# The other inputs (IPD, SLD, outcomes, sld_outcomes) are flat already and
# read with read.csv() or any other reader; see templates/new_study_template.R.

metadata_from_table <- function(df, sep = "|") {
  stopifnot(is.data.frame(df))
  missing_cols <- setdiff(METADATA_COLUMNS, names(df))
  if (length(missing_cols) > 0) {
    stop("metadata table is missing column(s): ", paste(missing_cols, collapse = ", "), ".", call. = FALSE)
  }

  m <- tibble::as_tibble(df)
  m$variable <- as.character(m$variable)
  m$type     <- as.character(m$type)
  for (col in METADATA_LOGICAL_COLUMNS) m[[col]] <- .as_flag(m[[col]], col)
  m$display_order <- as.numeric(m$display_order)
  m$level_order   <- lapply(as.character(m$level_order), .split_levels, sep = sep)

  validate_metadata(m)
}

read_metadata_csv <- function(path, sep = "|", ...) {
  metadata_from_table(utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"), ...), sep = sep)
}

.split_levels <- function(x, sep) {
  if (is.na(x) || !nzchar(trimws(x))) return(NULL)
  trimws(strsplit(x, sep, fixed = TRUE)[[1]])
}

.as_flag <- function(x, col) {
  if (is.logical(x)) return(x)
  key <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(key))
  out[key %in% c("true", "t", "1", "yes", "y")]  <- TRUE
  out[key %in% c("false", "f", "0", "no", "n")] <- FALSE
  bad <- unique(x[is.na(out) & !is.na(x)])
  if (length(bad) > 0) {
    stop("`", col, "` has value(s) that are not TRUE/FALSE: ", paste(bad, collapse = ", "), ".", call. = FALSE)
  }
  out
}
