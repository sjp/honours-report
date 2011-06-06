
# Data downloaded from
# http://www.gapminder.org/data/
# As Excel spreadsheets then saved as CSV
# This is the first plot in Hans Rosling's classic first TED talk

fert <- read.csv("indicatorundatatotal_fertility.csv")
life <- read.csv("indicatorlife_expectancy_at_birth.csv")
popn <- read.csv("indicatorgapminderpopulation.csv")

# Source : http://en.wikipedia.org/wiki/List_of_sovereign_states_and_dependent_territories_by_continent_%28data_file%29

# AF = Africa
# AS = Asia
# EU = Europe
# NA = North America
# SA = South America
# OC = Oceania
# AN = Antarctica

continents <- read.fwf("continent.txt",
                       widths=c(7, 8, 8, 8, 60),
                       na.strings="",
                       strip.white=TRUE)

# Just bother with 1950 onwards (when there is decent data)

fert <- fert[c(1, 152:211)]
life <- life[c(1, 152:211)]

# Just use 2009 popns for now

popn <- popn[c(1, 248)]

# 'life' has 5 more countries than 'fert' AND
# there are several inconsistencies, e.g.,
# "Bahamas, the" in 'life' BUT "Bahamas" in 'fert'
# See ...
#  life[, 1][!life[, 1] %in% fert[, 1]] 
#  fert[, 1][!fert[, 1] %in% life[, 1]]
# One country is unrecognizable, \xc5land, BUT it does have data

# The good news is that the "years" all line up
# See ...
#  all(names(fert)[-1] == names(life)[-1])

# Fill in NA's with subsequent non-NA
fillNA <- function(x) {
    n <- length(x)
    NAs <- is.na(x)
    whichNotNA <- which(!NAs)
    for (i in 1:n) {
        if (NAs[i]) {
            nextNotNA <- x[!NAs][min(which(whichNotNA > i))]
            # If there is no next non-NA
            # look left instead
            if (is.finite(nextNotNA)) {
                x[i] <- nextNotNA
            } else {
                prevNotNA <- x[!NAs][max(which(whichNotNA < i))]
                x[i] <- prevNotNA
            }
        }
    }
    x
}

filledFert <- cbind(fertility=fert[, 1],
                    as.data.frame(t(apply(fert[-1], 1, fillNA))))
filledLife <- cbind(expectancy=life[, 1],
                    as.data.frame(t(apply(life[-1], 1, fillNA))))

# For simplicity, just use countries in common for now

all <- merge(merge(filledFert, filledLife,
                   by.x="fertility",
                   by.y="expectancy"),
             popn, by.x="fertility", by.y="Population")

all$continent <- "AS"
for (i in 1:nrow(all)) {
    cont <- as.character(continents[, 1][grep(all[i, 1],
                                              continents[, 5])])
    if (length(cont) == 0)
        all$continent[i] <- NA
    else
        all$continent[i] <- cont
}

cols <- data.frame(continent=c("AS", "EU", "AF", "SA", "NA", "OC", "AN"),
                   colours=c("brown", "blue", "darkgreen", "orange", "darkred",
                     "cornflowerblue", "black"))
cols$colours <- as.character(cols$colours)

all <- merge(all, cols)

all <- all[all$continent != "AN", ]

all$radius <- sqrt(all$X2008)/max(sqrt(all$X2008))

all$fillColours <- adjustcolor(all$colours, alpha=.5)
