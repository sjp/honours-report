
vpError <- function() {
  stop("vp should only be path")
}

# Functions to take a grid grob and call appropriate
# functions from dev.R to produce output on a device

# Each function has to convert locations and dimenions
# into device coordinates THEN call the dev.R function

# Convert a unit object to a value in "device" units
# The calls to convert*() are just to get 'valueOnly'
# from "inches" units.
cx <- function(x, dev) {
  inchToDevX(convertX(x, "inches", valueOnly=TRUE), dev)
}

cy <- function(x, dev) {
  inchToDevY(convertY(x, "inches", valueOnly=TRUE), dev)
}

cw <- function(x, dev) {
  inchToDevX(convertWidth(x, "inches", valueOnly=TRUE), dev)
}

ch <- function(x, dev) {
  inchToDevY(convertHeight(x, "inches", valueOnly=TRUE), dev)
}

# Convert a "distance" (e.g., a circle radius)
cd <- function(x, dev) {
  pmin(inchToDevX(convertWidth(x, "inches", valueOnly=TRUE), dev),
       inchToDevY(convertHeight(x, "inches", valueOnly=TRUE), dev))
}

# Convert a gpar object to an device-neutral graphical parameter list
gparToDevPars <- function(gp) {
    devpar <- get.gpar()
    devpar[names(gp)] <- gp
    devpar
}

# Converting locations and widths
locToInches <- function(x, y, dev) {
  # Convert x and y to inches
  x <- convertX(x, "inches", valueOnly=TRUE)
  y <- convertY(y, "inches", valueOnly=TRUE)
  # Transform to inches on device
  n <- max(length(x), length(y))
  loc <- cbind(rep(x, length=n),
               rep(y, length=n),
               rep(1, length=n)) %*% current.transform()
  x <- unit(loc[,1]/loc[,3], "inches")
  y <- unit(loc[,2]/loc[,3], "inches")
  list(x=x, y=y)
}

dimToInches <- function(w, h, dev) {
  # FIXME:  Doesn't handle rotated viewports!!
  w <- convertWidth(w, "inches")
  h <- convertHeight(h, "inches")
  list(w=w, h=h)
}

dToInches <- function(d, dev) {
  w <- convertWidth(d, "inches", valueOnly=TRUE)
  h <- convertHeight(d, "inches", valueOnly=TRUE)
  d <- unit(pmin(w, h), "inches")
  d
}

# Generate (left, bottom) from (x, y), (width, height), and justification
leftbottom <- function(x, y, width, height, just, dev) {
  left <- switch(just[1],
                 left=x,
                 center=unit(convertX(x, "inches", valueOnly=TRUE) -
                   convertWidth(0.5*width, "inches", valueOnly=TRUE),
                   "inches"),
                 centre=unit(convertX(x, "inches", valueOnly=TRUE) -
                   convertWidth(0.5*width, "inches", valueOnly=TRUE),
                   "inches"),
                 right=unit(convertX(x, "inches", valueOnly=TRUE) -
                   convertWidth(width, "inches", valueOnly=TRUE),
                   "inches"),
                 # If it's "bottom" or "top" just use "centre" for
                 # horizontal align
                 unit(convertX(x, "inches", valueOnly=TRUE) -
                   convertWidth(0.5*width, "inches", valueOnly=TRUE),
                   "inches"))
  bottom <- switch(if (length(just) > 1) just[2] else just[1],
                   bottom=y,
                   center=unit(convertY(y, "inches", valueOnly=TRUE) -
                     convertHeight(0.5*height, "inches", valueOnly=TRUE),
                     "inches"),
                   centre=unit(convertY(y, "inches", valueOnly=TRUE) -
                     convertHeight(0.5*height, "inches", valueOnly=TRUE),
                     "inches"),
                   top=unit(convertY(y, "inches", valueOnly=TRUE) -
                     convertHeight(height, "inches", valueOnly=TRUE),
                     "inches"),
                   # If it's "left" or "right" then use "centre" for
                   # vertical align
                   unit(convertY(y, "inches", valueOnly=TRUE) -
                     convertHeight(0.5*height, "inches", valueOnly=TRUE),
                     "inches"))
  locToInches(left, bottom, dev)
}

# Generate hjust/vjust from just
justTohjust <- function(just) {
  if (is.na(match(just[1], c("left", "right"))))
    "centre"
  else
    just[1]
}

justTovjust <- function(just) {
  if (length(just) > 1) {
    just <- just[2]
  }
  if (is.na(match(just[1], c("top", "bottom"))))
    "centre"
  else
    just
}

# Grob to SVG
grobToDev <- function(x, dev) {
  UseMethod("grobToDev", x)
}

grobToDev.default <- function(x, dev) {
  stop("We shouldn't be here!")
}

grobToDev.grob <- function(x, dev) {
  depth <- 0
  if (!is.null(x$vp))
    if (!inherits(x$vp, "vpPath"))
      vpError()
    else {
      depth <- downViewport(x$vp)
      devStartGroup(devGrob(x$vp, dev), gparToDevPars(get.gpar()), dev)
    }
  primToDev(x, dev)
  if (depth > 0) {
    upViewport(depth)
    devEndGroup(dev)
  }
}

# grob to device grob
devGrob <- function(x, dev) {
  UseMethod("devGrob")
}

devGrob.default <- function(x, dev) {
  list(name=x$name)
}

devGrob.lines <- function(x, dev) {
  loc <- locToInches(x$x, x$y, dev)
  list(x=cx(loc$x, dev),
       y=cy(loc$y, dev),
       name=x$name)
}

devGrob.polygon <- function(x, dev) {
  loc <- locToInches(x$x, x$y, dev)
  list(x=cx(loc$x, dev),
       y=cy(loc$y, dev),
       name=x$name)
}

devGrob.pathgrob <- function(x, dev) {
    # The complication is converting the 'x', 'y', and 'id's
    # into lists
    if (is.null(x$id) && is.null(x$id.lengths)) {
        loc <- locToInches(x$x, x$y, dev)
        list(x=cx(loc$x, dev),
             y=cy(loc$y, dev),
             rule=x$rule,
             name=x$name)
    } else {
        if (is.null(x$id)) {
            n <- length(x$id.lengths)
            id <- rep(1L:n, x$id.lengths)
        } else {
            n <- length(unique(x$id))
            id <- x$id
        }
        listX <- split(x$x, id)
        listY <- split(x$y, id)
        listLoc <- mapply(locToInches, listX, listY, MoreArgs=list(dev),
                          SIMPLIFY=FALSE)
        list(x=lapply(listLoc,
               function(loc, dev) { cx(loc$x, dev) }, dev),
             y=lapply(listLoc,
               function(loc, dev) { cy(loc$y, dev) }, dev),
             rule=x$rule,
             name=x$name)
    }
}

devGrob.rect <- function(x, dev) {
  lb <- leftbottom(x$x, x$y, x$width, x$height, x$just, dev)
  dim <- dimToInches(x$width, x$height, dev)
  list(x=cx(lb$x, dev),
       y=cy(lb$y, dev),
       width=cw(dim$w, dev),
       height=ch(dim$h, dev),
       name=x$name)
}

devGrob.text <- function(x, dev) {
  loc <- locToInches(x$x, x$y, dev)
  list(x=cx(loc$x, dev),
       y=cy(loc$y, dev),
       text=x$label,
       hjust=justTohjust(x$just),
       vjust=justTovjust(x$just),
       rot=x$rot,
       name=x$name)  
}

devGrob.circle <- function(x, dev) {
  loc <- locToInches(x$x, x$y, dev)
  list(x=cx(loc$x, dev),
       y=cy(loc$y, dev),
       r=cd(dToInches(x$r), dev),
       name=x$name)
}

# Prim to Dev
primToDev <- function(x, dev) {
  UseMethod("primToDev")
}

primToDev.grob <- function(x, dev) {
}

primToDev.lines <- function(x, dev) {
  devLines(devGrob(x, dev), gparToDevPars(x$gp), dev)
}

# Any more efficient way of doing this?
# FIXME:  will lose any extra attributes of segments grob
primToDev.segments <- function(x, dev) {
  nx0 <- length(x$x0)
  nx1 <- length(x$x1)
  ny0 <- length(x$y0)
  ny1 <- length(x$y1)
  n <- max(nx0, nx1, ny0, ny1)
  for (i in 1:n) {
    lg <- linesGrob(unit.c(x$x0[(i-1) %% nx0 + 1],
                           x$x1[(i-1) %% nx1 + 1]),
                    unit.c(x$y0[(i-1) %% ny0 + 1],
                       x$y1[(i-1) %% ny1 + 1]))
    devLines(devGrob(lg, dev),
             gparToDevPars(x$gp), dev)
  }
}

primToDev.polygon <- function(x, dev) {
  devPolygon(devGrob(x, dev), gparToDevPars(x$gp), dev)
}

primToDev.pathgrob <- function(x, dev) {
  devPath(devGrob(x, dev), gparToDevPars(x$gp), dev)
}

primToDev.rect <- function(x, dev) {
  devRect(devGrob(x, dev), gparToDevPars(x$gp), dev)
}

primToDev.text <- function(x, dev) {
  devText(devGrob(x, dev), gparToDevPars(x$gp), dev)
}

primToDev.circle <- function(x, dev) {
  devCircle(devGrob(x, dev), gparToDevPars(x$gp), dev)
}

# Quick fix for now
# Add device method eventually?
# Could get tricky to do all symbol types here ... (?)
primToDev.points <- function(x, dev) {
    # ONLY pch = 1 or 3 handled
    if (!x$pch %in% c(1, 3))
        stop("Unsupported pch value")
    if (x$pch == 1) {
        devCircle(devGrob(circleGrob(x$x, x$y,
                                     0.5*x$size), dev),
                  gparToDevPars(x$gp), dev)
    } else if (x$pch == 3) { 
        # length of x and y already checked in grid.points
        n <- length(x$x)
        for (i in 1:n) {
            devLines(devGrob(linesGrob(unit.c(x$x[i] - 0.5*x$size,
                                              x$x[i] + 0.5*x$size),
                                       x$y[i]), dev),
                     gparToDevPars(x$gp), dev)
            devLines(devGrob(linesGrob(x$x[i],
                                       unit.c(x$y[i] - 0.5*x$size,
                                              x$y[i] + 0.5*x$size)), dev),
                     gparToDevPars(x$gp), dev)
        }
        
    }
}
  
primToDev.xaxis <- function(x, dev) {
  # If the at is NULL then the axis will have no
  # children;  need to be calculated on-the-fly
  if (is.null(x$at)) {
    at <- grid.pretty(current.viewport()$xscale)
    grobToDev(grid:::make.xaxis.major(at, x$main), dev)
    grobToDev(grid:::make.xaxis.ticks(at, x$main), dev)
    if (x$label)
      grobToDev(grid:::make.xaxis.labels(at, x$label, x$main), dev)
  } 
}

primToDev.yaxis <- function(x, dev) {
  # If the at is NULL then the axis will have no
  # children;  need to be calculated on-the-fly
  if (is.null(x$at)) {
    at <- grid.pretty(current.viewport()$yscale)
    grobToDev(grid:::make.yaxis.major(at, x$main), dev)
    grobToDev(grid:::make.yaxis.ticks(at, x$main), dev)
    if (x$label)
      grobToDev(grid:::make.yaxis.labels(at, x$label, x$main), dev)
  } 
}

grobToDev.gTree <- function(x, dev) {
  depth <- 0
  if (!is.null(x$vp))
    if (!inherits(x$vp, "vpPath"))
      vpError()
    else {
      depth <- downViewport(x$vp)
      devStartGroup(devGrob(x$vp, dev), gparToDevPars(get.gpar()), dev)
    }
  if (!is.null(x$childrenvp)) {
    pushViewport(x$childrenvp)
    upViewport(grid:::depth(x$childrenvp))
  }
  primToDev(x, dev)
  devStartGroup(devGrob(x, dev), gparToDevPars(x$gp), dev)
  lapply(x$children, grobToDev, dev)
  devEndGroup(dev)
  if (depth > 0) {
    upViewport(depth)
    devEndGroup(dev)
  }
}

# grid to SVG
# Given a gTree created by grid.grab()
gridToDev <- function(gTree, dev) {
  grobToDev(gTree, dev)
}


