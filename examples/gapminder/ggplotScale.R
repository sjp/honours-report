
calcGGplotScale <- function() {    
# Produce a ggplot with two points at
    print(ggplot(data.frame(x=0:1, y=0:1)) +
          geom_point(aes(x=x, y=y)))
# Get the points grob
    grob <- grid.get("geom_point", grep=TRUE)
# Determine extent of scaled region
    list(xrange=as.numeric(grob$x),
         yrange=as.numeric(grob$y))
}

ggScale <- calcGGplotScale()

xlimits <- ggScale$xrange
ylimits <- ggScale$yrange

ggplotScaleX <- function(x, minx=min(x), maxx=max(x)) {
    xlimits[1] + (x - minx)/(maxx - minx)*(xlimits[2] - xlimits[1])
}

ggplotScaleY <- function(x, minx=min(x), maxx=max(x)) {
    ylimits[1] + (x - minx)/(maxx - minx)*(ylimits[2] - ylimits[1])
}

