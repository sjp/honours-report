\name{grid.garnish}
\alias{grid.garnish}
\alias{garnishGrob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Associate arbitrary SVG attributes with a grid grob }
\description{
  Creates an svg.grob object which is a normal grid grob,
  with SVG attributes attached.  Useful in conjunction with
  \code{gridToSVG}, to produce an SVG document with
  attributes that have no corresponding concept in grid graphics.
}
\usage{
garnishGrob(x, ...)
grid.garnish(path, ..., redraw=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A grob.}
  \item{path}{ A grob path specifying a drawn grob. }
  \item{...}{ Arbitrary SVG attribute settings. }
  \item{redraw}{A logical indicating whether to redraw the current page.}
}
\value{
  An svg.grob object.
}
\author{ Paul Murrell }
\seealso{ \code{\link{gridToSVG}} }                           
\keyword{ dplot }% at least one, from doc/KEYWORDS

