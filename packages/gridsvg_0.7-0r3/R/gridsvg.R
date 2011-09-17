
# Functions to take a grid grob and call appropriate
# functions from svg.R to produce SVG output


# User function
gridToSVG <- function(name="Rplots.svg") {
  svgdev <- openSVGDev(name, width=par("din")[1], height=par("din")[2])
  # Create a gTree from the current page
  gTree <- grid.grab()
  # Convert gTree to SVG
  gridToDev(gTree, svgdev)
  devClose(svgdev)
}

old.gridToSVG <- function(name="Rplots.svg") {
  svgdev <- openSVGDev(name, width=par("din")[1], height=par("din")[2])
  # Start a new page because we are going to be reproducing the
  # pushing and popping of viewports and this needs to be done
  # from scratch 
  grid.newpage(recording=FALSE)
  # Traverse the grid display list producing
  # SVG equivalents of all grid output
  # This nastily peeks into the grid NAMESPACE to get the
  # display list (for now)
  lapply(grid:::grid.Call("L_getDisplayList"), gridToDev, svgdev)
  # Before closing, need to pop the top-level viewport
  # which is not possible in grid
  devEndGroup(svgdev)
  devClose(svgdev)
}
