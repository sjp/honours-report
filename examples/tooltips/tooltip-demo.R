library(gridSVG)

# General function to apply a function to each element of the
# grid display list - should end up in 'grid' itself
grid.DLapply <- function(fun, ...) {
    # Traverse DL and do something to each entry
    gridDL <- grid:::grid.Call.graphics("L_getDisplayList")
    gridDLindex <- grid:::grid.Call.graphics("L_getDLindex")
    for (i in 1:gridDLindex) {
        elt <- grid:::grid.Call.graphics("L_getDLelt", i)
        grid:::grid.Call.graphics("L_setDLindex", i)
        grid:::grid.Call.graphics("L_setDLelt", fun(elt, ...))
    }
    grid:::grid.Call.graphics("L_setDLindex", gridDLindex)
}

# Add tooltip attributes to a grob on the DL
garnishAllGrobs <- function(elt) {
    if (inherits(elt, "grob")) {
        garnishGrob(elt,
                    onmousemove=paste("showTooltip(evt, '",
                      gsub("\n", " ", elt$name), "')",
                      sep=""),
                    onmouseout="hideTooltip()")
    } else {
        elt
    }
}

addTooltips <- function(filename="Rplots.svg") {
    grid.DLapply(garnishAllGrobs)
    grid.script(filename="tooltip.js")
    gridToSVG(filename)
}


# Beginning graphics
grid.newpage()
grid.circle(x = c(0.2, 0.5), y = c(0.7, 0.5), r = 0.1,
            gp = gpar(fill = "black"), name = "testCircle")
grid.rect(x = 0.7, y = 0.2, width = 0.2, height = 0.2,
          gp = gpar(fill = "blue"), name = "testRect")
addTooltips("testTooltips.svg")

grid.newpage()
d <- rnorm(1000)
densityplot(~d)
addTooltips("testLatticeTooltips.svg")
