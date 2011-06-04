library(gridSVG)
library(ggplot2)

aapl <- read.csv("aapl.csv")
aapl$X.Date <- as.Date(aapl$X.Date, format="%d-%b-%y")
names(aapl)[1] <- "Date"

goog <- read.csv("goog.csv")
goog$X.Date <- as.Date(goog$X.Date, format="%d-%b-%y")
names(goog)[1] <- "Date"

amzn <- read.csv("amzn.csv")
amzn$X.Date <- as.Date(amzn$X.Date, format="%d-%b-%y")
names(amzn)[1] <- "Date"

msft <- read.csv("msft.csv")
msft$X.Date <- as.Date(msft$X.Date, format="%d-%b-%y")
names(msft)[1] <- "Date"

stockprices.df <- rbind(aapl, amzn, goog, msft)

qplot(Date, Close, data=stockprices.df, group=Code, geom="line", colour=Code, log="y")

# Find out what the name of the polyline is
grid.ls()

# Get the polyline
g <- grid.get("GRID.polyline.1")
gx <- split(g$x, g$id)
gy <- split(g$y, g$id)
m <- matrix("", nrow=length(gx[[1]]), ncol=length(unique(g$id)))

for (i in 1:length(unique(g$id))) {
  xs <- as.numeric(gx[[i]])
  ys <- as.numeric(gy[[i]])

  for (j in 1:length(xs)) {
    m[j, i] <- paste(c(xs[1:j], rep(xs[j], length(xs) - j)), ",",
                     c(ys[1:j], rep(ys[j], length(ys) - j)), " ", sep="", collapse="")
  }
}

grid.animate("GRID.polyline.1", points=m, duration=30, rep=TRUE)
gridToSVG("animatedStocks.svg")
