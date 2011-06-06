
source("gmdata.R")

continentCols <- cols$colours
names(continentCols) <- cols$continent

library(ggplot2)
library(gridSVG)

source("ggplotScale.R")

# Single-panel qplot
###########################
print(
ggplot(data=all) +
      geom_point(aes(x=X1950.x, y=X1950.y, size=radius,                    
                     colour=continent),
                 shape=16, alpha=.5) +
      scale_x_continuous(limits=c(1, 9),
                         name="Fertility\n(# children per woman)") +
      scale_y_continuous(limits=c(10, 90),
                         name="Life Expectancy (at birth)") +
      scale_colour_manual(values=continentCols) +
      opts(legend.position="none") +
      scale_size(to=c(1, 20))
)

xCols <- grep("[.]x$", names(all))
yCols <- grep("[.]y$", names(all))

# Try to animate
grid.set("geom_point", grep=TRUE,
         animateGrob(grid.get("geom_point", grep=TRUE),
                     duration=20,
                     x=t(ggplotScaleX(as.matrix(all[xCols]),
                                      1, 9)),
                     y=t(ggplotScaleY(as.matrix(all[yCols]),
                                      10, 90))))

gridToSVG("gapminderOnePanel.svg")

# Multi-panel lattice plot
###########################
x11(width=10, height=7)
print(
ggplot(data=all) +
      geom_point(aes(x=X1950.x, y=X1950.y, size=radius,                    
                     colour=continent),
                 shape=16, alpha=.5) +
      scale_x_continuous(limits=c(1, 9),
                         name="Fertility\n(# children per woman)") +
      scale_y_continuous(limits=c(10, 90),
                         name="Life Expectancy (at birth)") +
      scale_colour_manual(values=continentCols) +
      opts(legend.position="none") +
      facet_wrap(~ continent) +
      scale_size(to=c(1, 20))
)


# Try to animate
grobs <- grid.get("geom_point", grep=TRUE, global=TRUE)
# Mimick the ordering induced by facet_wrap
continents <- matrix(unique(all$continent), byrow=TRUE, ncol=3)
for (i in 1:length(continents)) {
    grid.set(grobs[[i]]$name,
             animateGrob(grobs[[i]],
                         duration=20,
                         x=t(ggplotScaleX(as.matrix(all[all$continent ==
                                                        continents[i], xCols]),
                                          1, 9)),
                         y=t(ggplotScaleX(as.matrix(all[all$continent ==
                                                        continents[i], yCols]),
                                          10, 90))),
             redraw=FALSE)
}

# Add text label for year
years <- 1950:2009
visibility <- matrix("hidden", nrow=length(years), ncol=length(years))
diag(visibility) <- "visible"
yearText <- animateGrob(garnishGrob(textGrob(years, .9, .05,
                                             name="year",
                                             gp=gpar(cex=2, col="grey")),
                                    visibility="hidden"),
                        duration=20,
                        visibility=visibility)
grid.draw(yearText)

gridToSVG("gapminderMultiPanel.svg")
dev.off()
