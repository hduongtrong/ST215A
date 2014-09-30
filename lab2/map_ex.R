# A slight modification to Hadley Wickham's response to the clorpleth challenge.
# http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html
# https://gist.github.com/hadley/233134

# See also:
# https://uchicagoconsulting.wordpress.com/tag/r-ggplot2-maps-visualization/
library(ggplot2)
library(maps)

# First (and most annoying) task - get matching state and county variables 
# for both datasets.  And unfortauntely it's not quite right, as you can
# see from the finish product - some counties are missing.
# Make sure your network is working before you do this.
unemp <- read.csv("http://datasets.flowingdata.com/unemployment09.csv",
                  header = F, stringsAsFactors = F)
names(unemp) <- c("id", "state_fips", "county_fips", "name", "year", 
                  "?", "?", "?", "rate")
unemp$county <- tolower(gsub(" County, [A-Z]{2}", "", unemp$name))
unemp$state <- gsub("^.*([A-Z]{2}).*$", "\\1", unemp$name)

# Load the outlines of the counties.
county_df <- map_data("county")
names(county_df) <- c("long", "lat", "group", "order", "state_name", "county")
county_df$state <- state.abb[match(county_df$state_name, tolower(state.name))]
county_df$state_name <- NULL

# Load the outlines of the states.
state_df <- map_data("state")

# Combine together.
choropleth <- merge(county_df, unemp, by = c("state", "county"))
choropleth <- choropleth[order(choropleth$order), ]
choropleth$rate_d <- cut(choropleth$rate, breaks = c(seq(0, 10, by = 2), 35))

# Once you have the data in the right format, recreating the plot is
# straightforward.  Try looking at these plots separately to see what they are
# doing.
p <- ggplot(data=NULL, aes(long, lat)) +
  geom_polygon(data= choropleth, aes(fill = rate_d, group = group),
               colour = "white", alpha=1/2, size = 0.2) + 
  geom_polygon(data = state_df, colour = "white", fill = NA, aes(group = group)) +
  scale_fill_brewer(palette = "PuRd")
print(p)

# Add an orange point on Berkeley
p + geom_point(data=NULL, aes(x = -122, y = 37, group="Berkeley"),
               color="orange", size=5)
