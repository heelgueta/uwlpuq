#install.packages("plotly")
library(plotly)

coordinates = read.csv("coords.csv")

fig <- coordinates 
fig <- fig %>%
  plot_ly(
    lat = ~lat,
    lon = ~lon,
    marker = list(color = "blue"),
    type = 'scattermapbox',
    mode = 'markers')
fig <- fig %>%
  layout(
    mapbox = list(
      style = 'open-street-map',
      zoom =12,
      center = list(lon = -70.9, lat = -53.15))) 

fig
