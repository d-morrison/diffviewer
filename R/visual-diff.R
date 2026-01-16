#' HTML widget to visually compare two files
#'
#' Currently supports:
#' * image diffs for `.svg` and `.png`
#' * tabular diffs for `.csv` and `.rds` files containing data.frames
#' * object diffs for `.rds` files containing other R objects (using waldo)
#' * text diffs for everything else
#'
#' @param file_old,file_new Paths to files to compare
#' @param width,height Output size
#' @seealso [visual_diff_output()] for use within Shiny apps
#' @returns A HTML widget
#' @export
#' @examples
#' path1 <- tempfile()
#' path2 <- tempfile()
#' writeLines(letters, path1)
#' writeLines(letters[-13], path2)
#'
#' if (interactive()) {
#'   visual_diff(path1, path2)
#' }
visual_diff <- function(file_old, file_new, width = NULL, height = NULL) {
  stopifnot(file.exists(file_old), file.exists(file_new))
  stopifnot(tolower(tools::file_ext(file_old)) == tolower(tools::file_ext(file_new)))

  ext <- tolower(tools::file_ext(file_old))
  
  # Special handling for .rds files
  if (ext == "rds") {
    old_obj <- readRDS(file_old)
    new_obj <- readRDS(file_new)
    
    # Check if both are data.frames
    if (is_data_frame(old_obj) && is_data_frame(new_obj)) {
      # Use CSV comparison (daff.js)
      # Convert data.frames to CSV format
      old_csv <- tempfile(fileext = ".csv")
      new_csv <- tempfile(fileext = ".csv")
      on.exit(unlink(c(old_csv, new_csv)), add = TRUE)
      
      utils::write.csv(old_obj, old_csv, row.names = FALSE)
      utils::write.csv(new_obj, new_csv, row.names = FALSE)
      
      old_raw <- read_raw(old_csv)
      new_raw <- read_raw(new_csv)
      
      widget_data <- list(
        old = paste0("data:text/csv;base64,", jsonlite::base64_enc(old_raw)),
        new = paste0("data:text/csv;base64,", jsonlite::base64_enc(new_raw)),
        filename = basename(file_old),
        typediff = "data"
      )
    } else if (is_plot_object(old_obj) && is_plot_object(new_obj)) {
      # For plot objects, convert to text representation
      old_text <- paste(utils::capture.output(print(old_obj)), collapse = "\n")
      new_text <- paste(utils::capture.output(print(new_obj)), collapse = "\n")
      
      widget_data <- list(
        old = old_text,
        new = new_text,
        filename = basename(file_old),
        typediff = "text"
      )
    } else {
      # Use waldo for everything else (non-plot, non-data.frame objects)
      comparison <- waldo::compare(old_obj, new_obj)
      
      if (length(comparison) == 0) {
        # Objects are identical - show the structure
        obj_text <- paste(utils::capture.output(utils::str(old_obj)), collapse = "\n")
        widget_data <- list(
          old = obj_text,
          new = obj_text,
          filename = basename(file_old),
          typediff = "text"
        )
      } else {
        # Objects differ - use waldo's comparison output
        # waldo returns a character vector; each element is a line of the comparison
        waldo_output <- paste(comparison, collapse = "\n")
        
        # Show waldo output in "old" and same in "new" to avoid additional diffing
        widget_data <- list(
          old = waldo_output,
          new = waldo_output,
          filename = basename(file_old),
          typediff = "text"
        )
      }
    }
  } else {
    widget_data <- list(
      old = file_data(file_old),
      new = file_data(file_new),
      filename = basename(file_old),
      typediff = file_type(file_old)
    )
  }

  htmlwidgets::createWidget(
    name = "visual_diff",
    widget_data,
    width = width,
    height = height,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      defaultHeight = "auto",
      knitr.figure = FALSE
    ),
    package = "diffviewer"
  )
}

#' Shiny bindings for `visual_diff()`
#'
#' Use `visual_diff_output()` in ui and `render_visual_diff(visual_diff(...))`
#' in the server function.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a visual_diff
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @export
#' @return Components for use inside a Shiny app.
#' @examples
#' if (require("shiny") && interactive()) {
#' ui <- fluidPage(
#'   visual_diff_output("diff")
#' )
#'
#' server <- function(input, output, session) {
#'   path1 <- tempfile()
#'   path2 <- tempfile()
#'   writeLines(letters, path1)
#'   writeLines(letters[-13], path2)
#'
#'   output$diff <- visual_diff_render(visual_diff(path1, path2))
#' }
#'
#' shinyApp(ui, server)
#' }
visual_diff_output <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(
    outputId,
    "visual_diff",
    width = width,
    height = height,
    package = "diffviewer"
  )
}

#' @rdname visual_diff_output
#' @export
visual_diff_render <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  }
  htmlwidgets::shinyRenderWidget(expr, visual_diff_output, env, quoted = TRUE)
}
