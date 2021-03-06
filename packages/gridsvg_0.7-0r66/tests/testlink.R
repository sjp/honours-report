
library(grid)
library(gridSVG)

# A very simple test
x11(width=6, height=6)
# Some default settings
pushViewport(viewport(gp=gpar(col="black", fill=NA)))
grid.text("Click me", name="txtgrob")
grid.hyperlink("txtgrob", href="http://cran.stat.auckland.ac.nz")
popViewport()

gridToSVG()
dev.off()

# A scatterplot and two dot plots designed to be
# linked together

# Some data
data(iris)

# A scatterplot of x vs y
x11(width=6, height=6)
# Some default settings
pushViewport(viewport(gp=gpar(col="black", fill=NA)))
pushViewport(plotViewport(c(5, 5, 4, 2)))
pushViewport(dataViewport(iris$Sepal.Length, iris$Sepal.Width))
grid.rect()
grid.xaxis(name="xaxis")
grid.yaxis(name="yaxis")
grid.hyperlink("xaxis", "dotx.html")
grid.hyperlink("yaxis", "doty.html")
grid.points(iris$Sepal.Length[iris$Species == "setosa"],
            iris$Sepal.Width[iris$Species == "setosa"],
            gp=gpar(col="red"))
grid.points(iris$Sepal.Length[iris$Species == "versicolor"],
            iris$Sepal.Width[iris$Species == "versicolor"],
            gp=gpar(col="green"))
grid.points(iris$Sepal.Length[iris$Species == "virginica"],
            iris$Sepal.Width[iris$Species == "virginica"],
            gp=gpar(col="blue"))
grid.text("Sepal Length", y=unit(-3, "lines"))
grid.text("Sepal Width", x=unit(-3, "lines"), rot=90)
popViewport(3)
gridToSVG("linkscatter.svg")
dev.off()

# A dot plot of x
x11(width=6, height=3)
# Some default settings
pushViewport(viewport(gp=gpar(col="black", fill=NA)))
pushViewport(plotViewport(c(5, 2, 4, 2)))
pushViewport(dataViewport(iris$Sepal.Length, yscale=c(0 ,1)))
grid.rect(gp=gpar(col="grey"))
grid.xaxis()
grid.points(iris$Sepal.Length[iris$Species == "setosa"],
            rnorm(50, .5, .05),
            size=unit(2, "char"),
            gp=gpar(col=NULL, fill="red", alpha=0.3))
grid.points(iris$Sepal.Length[iris$Species == "versicolor"],
            rnorm(50, .5, .05),
            size=unit(2, "char"),
            gp=gpar(col=NULL, fill="green", alpha=0.3))
grid.points(iris$Sepal.Length[iris$Species == "virginica"],
            rnorm(50, .5, .05),
            size=unit(2, "char"),
            gp=gpar(col=NULL, fill="blue", alpha=0.3))
grid.text("Sepal Length", y=unit(-3, "lines"))
grid.text(name="returnlink",
          "Return to Scatterplot",
          y=unit(1, "npc") + unit(1, "lines"),
          gp=gpar(col="grey"))
grid.hyperlink("returnlink", "slide17.html")
popViewport(3)
gridToSVG("linkdotx.svg")
dev.off()

# A dot plot of y
x11(width=6, height=3)
# Some default settings
pushViewport(viewport(gp=gpar(col="black", fill=NA)))
pushViewport(plotViewport(c(5, 2, 4, 2)))
pushViewport(dataViewport(iris$Sepal.Width, yscale=c(0 ,1)))
grid.rect(gp=gpar(col="grey"))
grid.xaxis()
grid.points(iris$Sepal.Width[iris$Species == "setosa"],
            rnorm(50, .5, .05),
            size=unit(2, "char"),
            gp=gpar(col=NULL, fill="red", alpha=0.3))
grid.points(iris$Sepal.Width[iris$Species == "versicolor"],
            rnorm(50, .5, .05),
            size=unit(2, "char"),
            gp=gpar(col=NULL, fill="green", alpha=0.3))
grid.points(iris$Sepal.Width[iris$Species == "virginica"],
            rnorm(50, .5, .05),
            size=unit(2, "char"),
            gp=gpar(col=NULL, fill="blue", alpha=0.3))
grid.text("Sepal Width", y=unit(-3, "lines"))
grid.text(name="returnlink",
          "Return to Scatterplot",
          y=unit(1, "npc") + unit(1, "lines"),
          gp=gpar(col="grey"))
grid.hyperlink("returnlink", "slide17.html")
popViewport(3)
gridToSVG("linkdoty.svg")
dev.off()


