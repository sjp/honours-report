\name{grid.animate}
\alias{grid.animate}
\alias{animateGrob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Animate a grid grob }
\description{
  Creates an animated.grob object.
  Useful in conjunction with
  \code{gridToSVG}, to produce an SVG document with
  animated graphical elements.
}
\synopsis{
animateGrob(grob, ...,
             duration=1, id="auto",
             rep=FALSE, revert=FALSE) 
grid.animate(path, ...) 
  }
\usage{
grid.animate(path, ...,
             duration=1, id="auto",
             rep=FALSE, revert=FALSE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{ A grob path specifying a drawn grob. }
  \item{\dots}{ Arguments of the grob to animate. }
  \item{duration}{ The duration in seconds of the animation. }
  \item{id}{ NULL or numeric to indicate multiple animated values
    (i.e., allows a vector of animated values).  If ``auto'', each
    unit in \code{x} and \code{...} is taken to be a separate animated
    value.}
  \item{rep}{ The number of times the animation should repeat.
    \code{FALSE} means once, \code{TRUE} means indefinitely. }
  \item{revert}{ What should happen when (if) the animation ends;
    \code{TRUE} means revert to the first animated value,
    \code{FALSE} means finish on the last animated value. }
}
\value{
  An animated.grob object.
}
\author{ Paul Murrell }
\seealso{ \code{\link{gridToSVG}} }                           
\keyword{ dplot }% at least one, from doc/KEYWORDS
