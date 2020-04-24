library(rjson)
library(png)
#setwd("~/folders/metoffice-radar/R")
############# Provided by Stefan Siegert #############

# some specs
metoffice.key <- 'a2ee40d3-c3fa-4d49-813a-ff0ab8c131db'
json.file <- "data/json/radar.json"

# get json meta file
get.url <- paste("http://datapoint.metoffice.gov.uk/public/data/layer/wxobs/all/json/capabilities?key=", metoffice.key, sep="")
download.file(url=get.url, destfile=json.file)

# build query urls for radar images
j <- fromJSON(readLines(json.file, warn=FALSE))
base.url <- j[["Layers"]][["BaseUrl"]][["$"]]
rain.layer <- j[["Layers"]][["Layer"]][[4]]
base.url <- sub("\\{LayerName\\}", rain.layer[["Service"]][["LayerName"]], base.url)
base.url <- sub("\\{ImageFormat\\}", rain.layer[["Service"]][["ImageFormat"]], base.url)
base.url <- sub("\\{key\\}", metoffice.key, base.url)
query.urls <- 
sapply(rain.layer[["Service"]][["Times"]][["Time"]], function(t) {
  sub("\\{Time\\}", t, base.url)})

# download data
times <- names(query.urls)
png.files <- paste("data/img/", times, ".png", sep="")
for (i in 1:length(times)) {
  if (!file.exists(png.files[i])) {
    download.file(url=query.urls[i], destfile=png.files[i])
  }
}

# raster png images to arrays
# raster.list <- lapply(png.files, readPNG)
# names(raster.list) <- times



