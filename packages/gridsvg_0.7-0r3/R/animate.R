
# duration says how many SECONDS the animation lasts for
# id indicates the identity of multiple animated values
#   (i.e., allows a vector of animated values)
#   If "auto" then it depends on the number and size
#     of the elements being animated.  If there is
#     only one element, it is NULL.
# rep says how many times to repeat the animation
#   (TRUE means indefinitely;  FALSE means once)
# revert says whether to revert to the start value of the
#   animation upon completion

autoid <- function(id) {
  if (!is.numeric(id))
    if (id == "auto")
      TRUE
    else
      stop("Invalid id")
  else
    FALSE
}

animateGrob <- function(grob, ...,
                        duration=1, id="auto",
                        rep=FALSE, revert=FALSE) {
  animations <- list(...)
  if (is.null(animations[[1]]))
    stop("need argument to animate")
  cl <- class(grob)
  grob$animations <- c(animations, list(duration=duration, id=id,
                    rep=rep, revert=revert))
  class(grob) <- c("animated.grob", cl)
  grob
}
  
grid.animate <- function(path, ...) {
  grid.set(path, animateGrob(grid.get(path), ...))
}

animate <- function(x, animation, dev) {
  UseMethod("animate")
}

animate.rect <- function(x, animation, dev) {
  dur <- x$animations$duration
  rep <- x$animations$rep
  rev <- x$animations$revert
  switch(animation,
         x={
           lb <- leftbottom(x$animations$x, x$y, x$width, x$height, x$just,
                            dev)
           svgAnimateXYWH("x", cx(lb$x, dev),
                          dur, rep, rev, x$name, dev@dev)
         },
         y={
           lb <- leftbottom(x$x, x$animations$y, x$width, x$height, x$just,
                            dev)
           svgAnimateXYWH("y", cy(lb$y, dev),
                          dur, rep, rev, x$name, dev@dev)
         },
         width={
           dim <- dimToInches(x$animations$width, x$height, dev)
           svgAnimateXYWH("width", cw(dim$width, dev),
                          dur, rep, rev, x$name, dev@dev)
         },
         height={
           dim <- dimToInches(x$animations$height, x$height, dev)
           svgAnimateXYWH("height", cw(dim$height, dev),
                          dur, rep, rev, x$name, dev@dev)
         })
}

animate.circle <- function(x, animation, dev) {
  dur <- x$animations$duration
  rep <- x$animations$rep
  rev <- x$animations$revert
  switch(animation,
         x={
           loc <- locToInches(x$animations$x, x$y, dev)
           svgAnimateXYWH("cx", cx(loc$x, dev),
                          dur, rep, rev, x$name, dev@dev)
         },
         y={
           loc <- locToInches(x$x, x$animations$y, dev)
           svgAnimateXYWH("cy", cy(loc$y, dev),
                          dur, rep, rev, x$name, dev@dev)
         })
}

animate.text <- function(x, animation, dev) {
  dur <- x$animations$duration
  rep <- x$animations$rep
  rev <- x$animations$revert
  # Special case if animating BOTH x and y
  if (all(c("x", "y") %in% names(x$animations))) {
    loc <- locToInches(x$animations$x, x$animations$y, dev)
    svgAnimateTranslation(cx(loc$x, dev), cy(loc$y, dev),
                          dur, rep, rev, x$name, dev@dev)
  } else {
    switch(animation,
           x={
             loc <- locToInches(x$animations$x, x$y, dev)
             svgAnimateTranslation(cx(loc$x, dev), cy(loc$y, dev),
                                   dur, rep, rev, x$name, dev@dev)
           },
           y={
             loc <- locToInches(x$x, x$animations$y, dev)
             svgAnimateTranslation(cx(loc$x, dev), cy(loc$y, dev),
                                   dur, rep, rev, x$name, dev@dev)
           })
  }
}

animate.lines <- function(x, animation, dev) {
  dur <- x$animations$duration
  rep <- x$animations$rep
  rev <- x$animations$revert
  # Special case if animating BOTH x and y
  if (all(c("x", "y") %in% names(x$animations))) {
    loc <- locToInches(x$animations$x, x$animations$y, dev)
    svgAnimatePoints(cx(loc$x, dev), cy(loc$y, dev), x$animations$id,
                     dur, rep, rev, x$name, dev@dev)
  } else {
    switch(animation,
           x={
             loc <- locToInches(x$animations$x, x$y, dev)

           },
           y={
             loc <- locToInches(x$x, x$animations$y, dev)

           })
  }
  
}


primToDev.animated.grob <- function(x, dev) {
  animations <- x$animations[!names(x$animations) %in%
                             c("duration", "id", "rep", "revert")]
  for (i in names(animations)) 
    animate(x, i, dev)
  NextMethod()
}

