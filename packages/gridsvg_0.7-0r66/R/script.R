
scriptGrob <- function(script=NULL, filename=NULL, type="text/ecmascript") {
  if(!is.null(filename)) {
    body <- paste(readLines(filename), collapse="\n")
  } else if (!is.null(script)) {
    body <- script;
  }
  sg <- grob(type = type, body = body, cl="script")
  sg
}

grid.script <- function(...) {
  grid.draw(scriptGrob(...))
}

grobToDev.script <- function(x, dev) {
  svgScript(x$body, x$type, svgdev=dev@dev)
}

