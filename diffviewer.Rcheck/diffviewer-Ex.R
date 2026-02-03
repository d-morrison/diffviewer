pkgname <- "diffviewer"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('diffviewer')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("visual_diff")
### * visual_diff

flush(stderr()); flush(stdout())

### Name: visual_diff
### Title: HTML widget to visually compare two files
### Aliases: visual_diff

### ** Examples

path1 <- tempfile()
path2 <- tempfile()
writeLines(letters, path1)
writeLines(letters[-13], path2)

if (interactive()) {
  visual_diff(path1, path2)
}



cleanEx()
nameEx("visual_diff_output")
### * visual_diff_output

flush(stderr()); flush(stdout())

### Name: visual_diff_output
### Title: Shiny bindings for 'visual_diff()'
### Aliases: visual_diff_output visual_diff_render

### ** Examples

if (require("shiny") && interactive()) {
ui <- fluidPage(
  visual_diff_output("diff")
)

server <- function(input, output, session) {
  path1 <- tempfile()
  path2 <- tempfile()
  writeLines(letters, path1)
  writeLines(letters[-13], path2)

  output$diff <- visual_diff_render(visual_diff(path1, path2))
}

shinyApp(ui, server)
}



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
