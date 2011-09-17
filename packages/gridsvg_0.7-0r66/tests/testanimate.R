library(grid)
library(gridSVG)

x11(width=6, height=6)
# Some default settings
pushViewport(viewport(gp=gpar(col="black", fill=NA)))

grid.rect(name="rect",
          x=0,
          y=0,
          just=c("left", "bottom"))
grid.animate("rect", x=unit(0:30, "mm"), duration=5, rep=TRUE)
grid.circle(name="circle",
            x=unit(0.5, "npc") + unit(0, "mm"),
            r=unit(10, "mm"))
grid.animate("circle", x=unit(0.5, "npc") + unit(0:30, "mm"),
             duration=5, rep=TRUE)
grid.text("hello", name="text1",
          x=unit(0.3, "npc") + unit(0, "mm"))
grid.animate("text1",
             x=unit(0.3, "npc") + unit(0:30, "mm"),
             duration=5, rep=TRUE)
grid.text("hello", name="text2",
          x=unit(0.3, "npc") + unit(0, "mm"),
          y=unit(0.3, "npc") + unit(0, "mm"))
grid.animate("text2",
             x=unit(0.3, "npc") + unit(0:30, "mm"),
             y=unit(0.3, "npc") + unit(0:30, "mm"),
             duration=5, rep=TRUE)

popViewport()

gridToSVG("animate.svg")
dev.off()
