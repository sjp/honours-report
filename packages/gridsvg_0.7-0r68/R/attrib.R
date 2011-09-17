
# Add arbitrary SVG attributes to a grob

garnishGrob <- function(x, ...) {
  cl <- class(x)
  # Should check that attributes are valid
  # Will need to be generic check with per-grob-type versions
  x$attributes <- list(...)
  class(x) <- c("svg.grob", cl)
  x
}

grid.garnish <- function(path, ..., grep=FALSE, redraw=FALSE) {
  grid.set(path, garnishGrob(grid.get(path, grep=grep), ...),
           grep=grep, redraw=redraw)
}

devGrob.svg.grob <- function(x, dev) {
  c(NextMethod(x), attributes=list(x$attributes))
}
