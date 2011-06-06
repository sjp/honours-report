library(grid)
library(gridSVG)

N <- 50
x <- rnorm(N)

mainvp <- viewport(width=.8,
                   xscale=range(x))
topvp <- viewport(y=.7, height=.2, just="bottom",
                  xscale=range(x))
midvp <- viewport(y=.5, height=.2, just="bottom",
                  xscale=range(x))
botvp <- viewport(y=.3, height=.2, just="bottom",
                  xscale=range(x))

grid.newpage()

# Main plot
pushViewport(mainvp)

pushViewport(topvp)
grid.rect(width=1.2, y=0, height=.5, just="bottom",
          gp=gpar(col=NA, fill="grey90"))
grid.xaxis()
popViewport()

pushViewport(midvp)
grid.rect(width=1.2, y=0, height=.5, just="bottom",
          gp=gpar(col=NA, fill="grey90"))
grid.xaxis()
popViewport()

pushViewport(botvp)
grid.rect(width=1.2, y=0, height=.5, just="bottom",
          gp=gpar(col=NA, fill="grey90"))
grid.xaxis()
popViewport()

grid.circle(unit(x, "native"),
            unit(.75, "npc"), r=unit(1, "mm"))

# Animation order
Nsample <- 10
sample <- sample(1:N, Nsample)

# Points fall
for (i in seq_along(sample)) {
    cg <- circleGrob(unit(x[sample[i]], "native"),
                     unit(.75, "npc"), r=unit(1, "mm"),
                     gp=gpar(col="red", fill=rgb(1, 0, 0, .2)))
    gcg <- garnishGrob(cg, visibility="hidden")
    acg <- animateGrob(gcg,
                       y=rep(c(.75, .55), c(i, Nsample - i + 1)),
                       visibility=rep(c("hidden", "visible"),
                         c(i - 1, Nsample - i + 2)),
                       duration=10)
    grid.draw(acg)
}

# Boxplot falls
summ <- summary(x[sample])
grid.rect(x=unit(summ[2], "native"),
          width=unit(summ[3] - summ[2], "native"),
          y=.55, height=.05, just="left",
          gp=gpar(col="blue", fill=rgb(0, 0, 1, .2)),
          name="rect1")
grid.rect(x=unit(summ[3], "native"),
          width=unit(summ[5] - summ[3], "native"),
          y=.55, height=.05, just="left",
          gp=gpar(col="blue", fill=rgb(0, 0, 1, .2)),
          name="rect2")
grid.garnish("rect1", visibility="hidden")
grid.garnish("rect2", visibility="hidden")
grid.animate("rect1",
             y=rep(c(.55, .35), c(Nsample + 1, 1)),
             visibility=rep(c("hidden", "visible"),
               c(Nsample, 2)),
             duration=12)
grid.animate("rect2",
             y=rep(c(.55, .35), c(Nsample + 1, 1)),
             visibility=rep(c("hidden", "visible"),
               c(Nsample, 2)),
             duration=12)

popViewport()


gridToSVG("wildAnim.svg")
