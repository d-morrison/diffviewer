read_raw <- function(file) {
  readBin(file, "raw", n = file.info(file)$size)
}

raw_to_utf8 <- function(data) {
  res <- rawToChar(data)
  Encoding(res) <- "UTF-8"
  gsub("\r", "", res, fixed = TRUE)
}

is.dir <- function(x) {
  file.info(x)$isdir
}

is_ggplot <- function(x) {
  inherits(x, "ggplot")
}

is_plot_object <- function(x) {
  # Check for common plot types
  is_ggplot(x) || inherits(x, c("recordedplot", "grob", "gTree"))
}

is_data_frame <- function(x) {
  inherits(x, "data.frame")
}

file_data <- function(path) {
  raw <- read_raw(path)
  ext <- tolower(tools::file_ext(path))

  filedata <- switch(ext,
    png = paste0("data:image/png;base64,", jsonlite::base64_enc(raw)),
    svg = paste0("data:image/svg+xml;base64,", jsonlite::base64_enc(raw)),
    csv = paste0("data:text/csv;base64,", jsonlite::base64_enc(raw)),
    raw_to_utf8(raw)
  )
}

rds_to_data <- function(path) {
  obj <- readRDS(path)
  
  # If it's a data.frame, export as CSV
  if (is_data_frame(obj)) {
    csv_path <- tempfile(fileext = ".csv")
    on.exit(unlink(csv_path), add = TRUE)
    utils::write.csv(obj, csv_path, row.names = FALSE)
    raw <- read_raw(csv_path)
    return(paste0("data:text/csv;base64,", jsonlite::base64_enc(raw)))
  }
  
  # If it's a plot object, convert to text representation
  if (is_plot_object(obj)) {
    text <- paste(utils::capture.output(print(obj)), collapse = "\n")
    return(text)
  }
  
  # For everything else, return NULL to signal we need special handling
  return(NULL)
}

file_type <- function(path) {
  switch(tolower(tools::file_ext(path)),
    png = ,
    svg = "image",
    csv = ,
    rds = "data",
    "text"
  )
}

rds_type <- function(path) {
  obj <- readRDS(path)
  
  # If it's a data.frame, use "data" type (for daff)
  if (is_data_frame(obj)) {
    return("data")
  }
  
  # For everything else (including plots), use "text" type
  return("text")
}
