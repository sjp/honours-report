

library(grid)
library(gridSVG)

# A very simple test
x11(width=6, height=6)
# Test script chunk
grid.script(file="test.script")
# Some default settings
pushViewport(viewport(gp=gpar(col="black", fill=NA)))
grid.circle(r=0.1, gp=gpar(fill="red"), name="circgrob")
# Test setting SVG attribute
grid.garnish("circgrob", onclick="circle_click(evt)")
popViewport()

gridToSVG()
dev.off()


