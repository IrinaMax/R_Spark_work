# Let's make the data tidy. #https://campus.datacamp.com/courses/education-data-analysis-primer-r-dplyr-and-plotly/getting-fancy-with-plotly?ex=2
#Load the tidyr library
library(tidyr)
#Gather the data
GatheredStudentData <-StudentData %>% gather(Indicator,Score, -SID,-First,-Last)
GatheredStudentData%>%summary
# Remove NA's
GatheredStudentData <- GatheredStudentData %>% na.omit()

# Dump the student data
glimpse(GatheredStudentData)
#------------------------------------------------------------------------
# Plotly provides online graphing, analytics, and statistics tools. Using their technology anyone, including yourself, can make beautiful, interactive web-based graphs.
# load the `plotly` package
library(plotly)

# This will create your very first plotly visualization
plot_ly(z = ~volcano)
#------------------------------------------------------------------------
# The diamonds dataset Plotly diamonds are forever
# You'll use several datasets throughout the tutorial to showcase the power of plotly. In the next exercises you will make use of the diamond dataset. A dataset containing the prices and other attributes of 1000 diamonds.
str(diamonds)

# A firs scatterplot has been made for you
plot_ly(diamonds, x = ~carat, y = ~price)

# Replace ___ with the correct vector
plot_ly(diamonds, x = ~carat, y = ~price, color = ~carat)

# Replace ___ with the correct vector
plot_ly(diamonds, x = ~carat, y = ~price, color = ~carat, size = ~carat)
#------------------------------------------------------------------------
#The interactive bar chart
str(diamonds)
# Calculate the numbers of diamonds for each cut<->clarity combination
diamonds_bucket <- diamonds %>% count(clarity, cut)
diamonds_bucket
# Replace ___ with the correct vector
plot_ly(diamonds_bucket, x = ~cut, y = ~n, type = "bar", color = ~clarity)
# Box plots
# The Non Fancy Box Plot
plot_ly(y = ~rnorm(50), type = "box")

# The Fancy Box Plot
plot_ly(diamonds, y = ~price, color = cut, type ="box")

# The Super Fancy Box Plots
plot_ly(diamonds, x = ~price, y = ~clarity, color =cut, type = "box") %>% layout(boxmode = "group")
plot_ly(diamonds, x = ~price, y = ~clarity, color =~clarity, type = "box") %>% layout(boxmode = "group")
plot_ly(diamonds, x = ~cut, y = ~price, color =~clarity, type = "box") %>% layout(boxmode = "group")
plot_ly(diamonds, x = ~cut, y = ~price, color = ~clarity, type = "box") %>%layout(boxmode = "group")
#-----------------------------------------------------------------------------------------------------
# Load the `plotly` library
library(plotly)

# Your volcano data
str(volcano)

# The heatmap
plot_ly(z = ~volcano, type = "heatmap")

# The 3d surface map
plot_ly(z = ~volcano, type = "surface")
#----------------------------------------------------------------------------
# ggplot2, the interactive dimension
# Create the ggplot2 graph
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) + geom_point()

# Make your plot interactive

qplot(wt, mpg, data = mtcars, color = ~cyl)+ggplotly()
#---------------------------------------------------------------------------
# Most Trafficked US Airports

# Most Trafficked US Airports
g <- list(
  scope = 'usa',
  showland = TRUE,
  landcolor = toRGB("gray95")
)
g
plot_geo(airport_traffic, lat = ~lat, lon = ~long) %>%
  add_markers(
    text = ~paste(airport, city, state, paste("Arrivals:", cnt), sep = "<br />"),
    color = ~cnt, symbol = I("square"), size = I(8), hoverinfo = "text"
  ) %>%
  colorbar(title = "Incoming flights<br />February 2011") %>%
  layout(
    title = 'Most trafficked US airports<br />(Hover for airport)', geo = g
  )


# Commercial Airports WorldWide
str(airports)
data(airports)
# Mapping all commercial airports in the world
g <- list(
  scope = 'world',
  showland = TRUE,
  landcolor = toRGB("gray95")
)

plot_geo(airports, lat = ~Latitude, lon = ~Longitude) %>%
  add_markers(
    text = ~paste(AirportID, City, Country, sep = "<br />"),
    color = ~Country, symbol = I("circle"), size = I(3), hoverinfo = "text", colors = "Set1"
  ) %>%
  layout(
    title = 'Commercial Airports Worldwide', geo = g
  )

