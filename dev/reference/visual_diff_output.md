# Shiny bindings for `visual_diff()`

Use `visual_diff_output()` in ui and
`render_visual_diff(visual_diff(...))` in the server function.

## Usage

``` r
visual_diff_output(outputId, width = "100%", height = "400px")

visual_diff_render(expr, env = parent.frame(), quoted = FALSE)
```

## Arguments

- outputId:

  output variable to read from

- width, height:

  Must be a valid CSS unit (like `'100%'`, `'400px'`, `'auto'`) or a
  number, which will be coerced to a string and have `'px'` appended.

- expr:

  An expression that generates a visual_diff

- env:

  The environment in which to evaluate `expr`.

- quoted:

  Is `expr` a quoted expression (with
  [`quote()`](https://rdrr.io/r/base/substitute.html))? This is useful
  if you want to save an expression in a variable.

## Value

Components for use inside a Shiny app.

## Examples

``` r
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
#> Loading required package: shiny
```
