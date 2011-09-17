
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
  grid.set(path, animateGrob(grid.get(path), ...), redraw=FALSE)
}

animate <- function(x, animation, dev) {
  UseMethod("animate")
}

animate.rect <- function(x, animation, dev) {

  # We may be dealing with multiple rects that need animating
  n <- max(length(x$x), length(x$y), length(x$width), length(x$height))

  # Repeating animation parameters so that each element can have
  # distinct values
  dur <- rep(x$animations$duration, length.out = n)
  rep <- rep(x$animations$rep, length.out = n)
  rev <- rep(x$animations$revert, length.out = n)

  for (i in 1:n) {
    subName <- subGrobName(x$name, i)

    switch(animation,
           x={
             if (! is.matrix(x$animations$x))
               x$animations$x <- matrix(x$animations$x)
             xunit <- attr(x$x, "unit")
             lb <- leftbottom(unit(x$animations$x[,i], xunit), x$y, x$width, x$height, x$just,
                              dev)
             svgAnimateXYWH("x", cx(lb$x, dev),
                            dur[i], rep[i], rev[i], subName, dev@dev)
           },
           y={
             if (! is.matrix(x$animations$y))
               x$animations$y <- matrix(x$animations$y)
             yunit <- attr(x$y, "unit")
             lb <- leftbottom(x$x, unit(x$animations$y[,i], yunit), x$width, x$height, x$just,
                              dev)
             svgAnimateXYWH("y", cy(lb$y, dev),
                            dur[i], rep[i], rev[i], subName, dev@dev)
           },
           width={
             if (! is.matrix(x$animations$width))
               x$animations$width <- matrix(x$animations$width)
             wunit <- attr(x$width, "unit")
             dim <- dimToInches(unit(x$animations$width[,i], wunit), x$height, dev)
             svgAnimateXYWH("width", cw(dim$w, dev),
                            dur[i], rep[i], rev[i], subName, dev@dev)
           },
           height={
             if (! is.matrix(x$animations$height))
               x$animations$height <- matrix(x$animations$height)
             hunit <- attr(x$height, "unit")
             dim <- dimToInches(x$width, unit(x$animations$height[,i], hunit), dev)
             svgAnimateXYWH("height", ch(dim$h, dev),
                            dur[i], rep[i], rev[i], subName, dev@dev)
           },
           # Any other attribute
           {
               if (! is.matrix(x$animations[[animation]]))
                   x$animations[[animation]] <-
                       matrix(x$animations[[animation]])
               svgAnimate(animation,
                          paste(x$animations[[animation]][,i],
                                collapse=";"),
                          dur[i], rep[i], rev[i], subName, dev@dev)
           })
  }
}

animate.circle <- function(x, animation, dev) {

  # We may be dealing with multiple circles that need animating
  n <- max(length(x$x), length(x$y), length(x$r))

  # Repeating animation parameters so that each element can have
  # distinct values
  dur <- rep(x$animations$duration, length.out = n)
  rep <- rep(x$animations$rep, length.out = n)
  rev <- rep(x$animations$revert, length.out = n)

  # Because grobs can produce multiple elements, if animation is to
  # occur on a grob it is assumed to occur on all elements, but
  # elements may simply have their properties assigned to the same
  # value multiple times.
  #
  # Also note that when casting to a matrix, units lose their "unit"
  # attribute, we have to set this to the same unit as the grob
  # attribute that is being animated, for this reason, attributes should
  # be in the same unit prior to calling grid.animate()
  for (i in 1:n) {
    subName <- subGrobName(x$name, i)

    switch(animation,
           x={
               if (! is.matrix(x$animations$x))
                   x$animations$x <- matrix(x$animations$x)
               xunit <- attr(x$x, "unit")
               loc <- locToInches(unit(x$animations$x[,i], xunit), x$y[i], dev)
               svgAnimateXYWH("cx", cx(loc$x, dev),
                              dur[i], rep[i], rev[i], subName, dev@dev)
           },
           y={
               if (! is.matrix(x$animations$y))
                   x$animations$y <- matrix(x$animations$y)
               yunit <- attr(x$y, "unit")
               loc <- locToInches(x$x[i], unit(x$animations$y[,i], yunit), dev)
               svgAnimateXYWH("cy", cy(loc$y, dev),
                              dur[i], rep[i], rev[i], subName, dev@dev)
           },
           r={
               if (! is.matrix(x$animations$r))
                   x$animations$r <- matrix(x$animations$r)
               runit <- attr(x$r, "unit")
               svgAnimateXYWH("r", cd(unit(x$animations$r[,i], runit), dev),
                              dur[i], rep[i], rev[i], subName, dev@dev)
           },
           # Any other attribute
           {
               if (! is.matrix(x$animations[[animation]]))
                   x$animations[[animation]] <-
                       matrix(x$animations[[animation]])
               svgAnimate(animation,
                          paste(x$animations[[animation]][,i],
                                collapse=";"),
                          dur[i], rep[i], rev[i], subName, dev@dev)
           })
  }
}

animate.points <- function(x, animation, dev) {

  # We may be dealing with multiple points that need animating
  n <- max(length(x$x), length(x$y), length(x$size))

  # These can differ for points
  pchs <- rep(x$pch, length.out = n)
  sizes <- rep(x$size, length.out = n)

  # Repeating animation parameters so that each element can have
  # distinct values
  dur <- rep(x$animations$duration, length.out = n)
  rep <- rep(x$animations$rep, length.out = n)
  rev <- rep(x$animations$revert, length.out = n)

  # Because grobs can produce multiple elements, if animation is to
  # occur on a grob it is assumed to occur on all elements, but
  # elements may simply have their properties assigned to the same
  # value multiple times.
  #
  # Also note that when casting to a matrix, units lose their "unit"
  # attribute, we have to set this to the same unit as the grob
  # attribute that is being animated, for this reason, attributes should
  # be in the same unit prior to calling grid.animate()
  for (i in 1:n) {
    subName <- subGrobName(x$name, i)

    switch(animation,
       x={
         if (! is.matrix(x$animations$x))
           x$animations$x <- matrix(x$animations$x)
         xunit <- attr(x$x, "unit")
         loc <- locToInches(unit(x$animations$x[,i], xunit), x$y[i], dev)

         if (pchs[i] == 1 || pchs[i] == 16)
           animattr <- "cx"
         else
           animattr <- "x"
         svgAnimateXYWH(animattr, cx(loc$x, dev),
                        dur[i], rep[i], rev[i], subName, dev@dev)
       },
       y={
         if (! is.matrix(x$animations$y))
           x$animations$y <- matrix(x$animations$y)
         yunit <- attr(x$y, "unit")
         loc <- locToInches(x$x[i], unit(x$animations$y[,i], yunit), dev)

         if (pchs[i] == 1 || pchs[i] == 16)
           animattr <- "cy"
         else
           animattr <- "y"
         svgAnimateXYWH(animattr, cy(loc$y, dev),
                        dur[i], rep[i], rev[i], subName, dev@dev)
       },
       size={
         if (! is.matrix(x$animations$size))
           x$animations$size <- matrix(x$animations$size)
         sunit <- attr(sizes, "unit")
         pointsize <- cd(unit(x$animations$size[,i], sunit), dev)

         if (pchs[i] == 0) {
           svgAnimateXYWH("width", pointsize,
                          dur[i], rep[i], rev[i], subName, dev@dev)
           svgAnimateXYWH("height", pointsize,
                          dur[i], rep[i], rev[i], subName, dev@dev)
         }

         if (pchs[i] == 1 || pchs[i] == 16) {
           svgAnimateXYWH("r", pointsize,
                          dur[i], rep[i], rev[i], subName, dev@dev)
         }
       })
  }
}

animate.text <- function(x, animation, dev) {
    
    # We may be dealing with multiple points that need animating
    n <- max(length(x$x), length(x$y), length(x$label))

    # Repeating animation parameters so that each element can have
    # distinct values
    dur <- rep(x$animations$duration, length.out = n)
    rep <- rep(x$animations$rep, length.out = n)
    rev <- rep(x$animations$revert, length.out = n)

    for (i in 1:n) {
        subName <- subGrobName(x$name, i)

        # Special case if animating BOTH x and y
        if (all(c("x", "y") %in% names(x$animations))) {
            if (! is.matrix(x$animations$x))
                x$animations$y <- matrix(x$animations$x)
            if (! is.matrix(x$animations$y))
                x$animations$y <- matrix(x$animations$y)
            loc <- locToInches(x$animations$x[,i], x$animations$y[,i], dev)
            svgAnimateTranslation(cx(loc$x, dev), cy(loc$y, dev),
                                  dur, rep, rev, x$name, dev@dev)
        } else {
            switch(animation,
                   x={
                       if (! is.matrix(x$animations$x))
                           x$animations$y <- matrix(x$animations$x)
                       loc <- locToInches(x$animations$x[,i], x$y[i], dev)
                       svgAnimateTranslation(cx(loc$x, dev), cy(loc$y, dev),
                                             dur, rep, rev, x$name, dev@dev)
                   },
                   y={
                       if (! is.matrix(x$animations$y))
                           x$animations$y <- matrix(x$animations$y)
                       loc <- locToInches(x$x[i], x$animations$y[,i], dev)
                       svgAnimateTranslation(cx(loc$x, dev), cy(loc$y, dev),
                                             dur, rep, rev, x$name, dev@dev)
                   },
                   # Any other attribute
                   {
                       if (! is.matrix(x$animations[[animation]]))
                           x$animations[[animation]] <-
                               matrix(x$animations[[animation]])
                       svgAnimate(animation,
                                  paste(x$animations[[animation]][,i],
                                        collapse=";"),
                                  dur[i], rep[i], rev[i], subName, dev@dev)
                   })
        }
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

animate.polyline <- function(x, animation, dev) {
  # If we only have one line
  if (is.null(x$id) && is.null(x$id.lengths)) {
      x$id <- rep(1L, length(x$x))
  }

  # Multiple lines exist
  if (is.null(x$id)) {
      n <- length(x$id.lengths)
      id <- rep(1L:n, x$id.lengths)
  } else {
      n <- length(unique(x$id))
      id <- x$id
  }

  # Repeating animation parameters so that each element can have
  # distinct values
  dur <- rep(x$animations$duration, length.out = n)
  rep <- rep(x$animations$rep, length.out = n)
  rev <- rep(x$animations$revert, length.out = n)

  for (i in 1:n) {
    subName <- subGrobName(x$name, i)

    switch(animation,
           points={
             # This is a bit of a special case to allow a line to "grow"
             # over time. Specified as a character matrix, with a column
             # for each line, but each row contains the points needed to
             # draw a line at each step.
             # The format of the line is "x1,y1 x2,y2 ... xn,yn"
             xunit <- attr(x$x, "unit")
             yunit <- attr(x$y, "unit")
             rows <- nrow(x$animations$points)
             pointsValues <- character(0)

             for (j in 1:rows) {
               rowPoints <- x$animations$points[j, i]
               vals <- as.numeric(strsplit(rowPoints, "[, ]")[[1]])
               nvals <- length(vals)
               xvals <- vals[seq(1, nvals, by = 2)]
               yvals <- vals[seq(2, nvals, by = 2)]
               xvals <- unit(xvals, xunit)
               yvals <- unit(yvals, yunit)
               loc <- locToInches(xvals, yvals, dev)
               xs <- cx(loc$x, dev)
               ys <- cy(loc$y, dev)

               pointsValues <- c(pointsValues, paste(xs, ",", ys, " ", sep="", collapse=""))
             }
             svgAnimate(animation,
                        paste(pointsValues, collapse=";"),
                        dur[i], rep[i], rev[i], subName, dev@dev)
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

