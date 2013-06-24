library(spatstat)
library(maps)
library(maptools)

# Make a random set of lat-lon pairs with values
usdat <- data.frame(x=runif(50, -115, -85), y=runif(50, 33, 41), z=runif(50, 
                                                                         0, 100))

map('usa')
points(usdat$x, usdat$y)  # do they fall in there?

library(maps)
map("state", interior = TRUE)
map("state", boundary = FALSE, col="gray", add = TRUE)
?map

library(ggplot2)
example(map_data)

data(zipcode)
head(zipcode)
str(zipcode)

if (require("maps")) {
  states <- map_data("state")
  arrests <- USArrests
  names(arrests) <- tolower(names(arrests))
  arrests$region <- tolower(rownames(USArrests))
  
  choro <- merge(states, arrests, sort = FALSE, by = "region")
  choro <- choro[order(choro$order), ]
  qplot(long, lat, data = choro, group = group, fill = assault,
        geom = "polygon")
  qplot(long, lat, data = choro, group = group, fill = assault / murder,
        geom = "polygon")
}

head()