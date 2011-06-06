
# From Wolfgang Huber
# Fig.1 in 
# http://www.ebi.ac.uk/~huber/pub/E-GEOD-5130/QMreport.html
# Have a try hovering your mouse over Figs. 5

# There is a local copy of the web page in ~/Teaching/Kim/Huber/

library(grid)
library(lattice)
library(gridSVG)

PC1 <- runif(10, -40, 10)
PC2 <- runif(10, -20, 20)
group <- factor(sample(c("Estrogen Receptor Negative",
                         "Estrogen Receptor Positive"),
                       10, replace=TRUE))

customPanel <- function(x, y, groups, ...) {
    grps <- levels(groups)
    for (i in 1:length(grps)) {
        index <- which(groups == grps[i])
        xx <- x[index]
        yy <- y[index]
        for (j in 1:length(xx)) {
            grid.circle(xx[j], yy[j], r=unit(1, "mm"),
                        default.unit="native",
                        name=paste("point", index[j], sep="."),
                        gp=gpar(col=NA,
                          fill=trellis.par.get("superpose.symbol")$col[i]))
        }
    }
}

xyplot(PC2 ~ PC1, group=group,
       panel=customPanel,
       key=list(rect=list(col=trellis.par.get("superpose.symbol")$col[1:2]),
         text=list(label=levels(group))))

for (i in 1:10) {
    grid.text(paste("ARRAY", i), x=.1, y=.01, just=c("left", "bottom"),
              name=paste("label", i, sep="."),
              gp=gpar(fontface="bold.italic"))
}

for (i in 1:10) {
    grid.garnish(paste("point", i, sep="."), 
                 onmouseover=paste('highlight(', i, '.1)', sep=""),
                 onmouseout=paste('dim(', i, '.1)', sep=""))
    grid.garnish(paste("label", i, sep="."),
                 visibility="hidden")
}

grid.script(file="huber.js")

gridToSVG("huber.svg")
