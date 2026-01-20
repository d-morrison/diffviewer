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

file_data <- function(path) {
  ext <- tolower(tools::file_ext(path))
  
  if (ext == "rds") {
    # Read RDS file with error handling
    obj <- tryCatch(
      readRDS(path),
      error = function(e) {
        stop("Failed to read .rds file: ", path, "\n", e$message, call. = FALSE)
      }
    )
    
    # Return the R object itself (will be compared by waldo in visual_diff)
    return(obj)
  }
  
  raw <- read_raw(path)

  filedata <- switch(ext,
    png = paste0("data:image/png;base64,", jsonlite::base64_enc(raw)),
    svg = paste0("data:image/svg+xml;base64,", jsonlite::base64_enc(raw)),
    csv = paste0("data:text/csv;base64,", jsonlite::base64_enc(raw)),
    raw_to_utf8(raw)
  )
}

file_type <- function(path) {
  switch(tolower(tools::file_ext(path)),
    png = ,
    svg = "image",
    csv = "data",
    rds = "rds",
    "text"
  )
}
