\name{grid.script}
\alias{grid.script}
\alias{scriptGrob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create a grid grob containing an SVG script}
\description{
  Creates a script object which is a normal grid grob
  containing an SVG script. Useful in conjunction with
  \code{gridToSVG}, to produce an SVG document with
  script elements.
}
\synopsis{
scriptGrob(script=NULL, filename=NULL, type="text/ecmascript")
grid.script(...)  
  }
\usage{
grid.script(script=NULL, filename=NULL, type="text/ecmascript")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{script}{ A character value specifying script code (no tags!). }
  \item{filename}{ A file from which to read script code (no tags!). }
  \item{type}{ The type of the script code. }
}
\value{
  A script object.
}
\author{ Paul Murrell }
\seealso{ \code{\link{gridToSVG}} }                           
\keyword{ dplot }% at least one, from doc/KEYWORDS

