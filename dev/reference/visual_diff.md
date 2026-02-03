# HTML widget to visually compare two files

Currently supports:

- image diffs for `.svg` and `.png`

- tabular diffs for `.csv`

- text diffs for everything else

## Usage

``` r
visual_diff(file_old, file_new, width = NULL, height = NULL)
```

## Arguments

- file_old, file_new:

  Paths to files to compare

- width, height:

  Output size

## Value

A HTML widget

## See also

[`visual_diff_output()`](https://diffviewer.r-lib.org/dev/reference/visual_diff_output.md)
for use within Shiny apps

## Examples

``` r
path1 <- tempfile()
path2 <- tempfile()
writeLines(letters, path1)
writeLines(letters[-13], path2)

if (interactive()) {
  visual_diff(path1, path2)
}
```
