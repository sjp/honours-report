library(gridSVG)
library(lattice)

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
    if (inherits(elt, "frame")) {
        garnishGrob(elt,
                    onmousedown=paste("mouseDown(evt, '", elt$name,"')", sep=""),
                    onmousemove="mouseMove(evt)"),
                    onmouseup="mouseUp(evt)")
    } else if (inherits(elt, "grob")) {
        garnishGrob(elt, onmousemove="mouseMove(evt)")
    } else {
       elt
    }
}

moveLegend <- function(filename="Rplots.svg") {
    grid.DLapply(garnishAllGrobs)
    grid.script(filename="legend.js")
    gridToSVG(filename)
}


# Beginning graphics
grid.newpage()

old.settings <- trellis.par.get()
## changing settings to new 'theme'
trellis.par.set(theme = col.whitebg())

xyplot(Petal.Length~Petal.Width, data = iris, groups=Species, 
       panel = panel.superpose,
       type = c("p", "smooth"), span=.75,
       col.line = trellis.par.get("strip.background")$col,
       col.symbol = trellis.par.get("strip.shingle")$col,
       key = list(title = "Iris Data", x = .15, y=.85, corner = c(0,1),
                  border = TRUE, 
                  points = list(col=trellis.par.get("strip.shingle")$col[1:3],
                  pch = trellis.par.get("superpose.symbol")$pch[1:3],
                  cex = trellis.par.get("superpose.symbol")$cex[1:3]
                  ),
       text = list(levels(iris$Species))))

trellis.par.set(theme = old.settings, strict = 2)

moveLegend("testLatticeLegend.svg")
