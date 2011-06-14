library(gridSVG)
grid.newpage()
grid.rect(width = 0.5, height = 0.5,
          gp = gpar(fill = "black"))
grid.garnish("GRID.rect.1",
             onmouseover = "setYellow(evt)")
grid.script(filename = "hover.js")
gridToSVG("hover.svg")
