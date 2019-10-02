### project arctic coordinates to normal xy
# clean dir to start
wd <- 'arcticPlotting'
dir.create(wd)
setwd(wd)
rm(wd)

# lon/lats into xy coords (radius = lat)
coord_lonlat_xy_transform <- function(lon = NULL, lat = NULL) {
  radius <- 90-lat
  
  max90degrees <- lon
  
  for(i in 1:length(max90degrees)) {
    while(max90degrees[i] > 90) {
      max90degrees[i] <- max90degrees[i] - 90
    }
  }
  
  phi <- max90degrees * pi/180
  
  x <- radius * cos( phi )
  y <- radius * sin( phi )
  
  for(i in 1:length(lon)) {
    
    if(lon[i] >= 0 && lon[i] <= 90 ) {
      yt <- y[i]
      y[i] <- x[i]
      x[i] <- -yt
    }
  
    if(lon[i] > 90 && lon[i] <= 180 ) {
      x[i] <- -x[i]
      y[i] <- -y[i]
    }
    
    if(lon[i] > 180 && lon[i] <= 270 ) {
      yt <- y[i]
      y[i] <- -x[i]
      x[i] <- yt
    }
    
    if(lon[i] > 270 && lon[i] <= 360 ) {

    }
  }
  out <- data.frame(x,y)
}

### test stuff
lats <- runif(50,71,90)
lons <- runif(50,1,360)

lats <- NULL
lats[1:50] <- 82 
lons <- seq(1,350,by=7)

xycoords <- coord_lonlat_xy_transform(lon = lons, lat = lats)
plot(xycoords)

rm(lats,lons,xycoords)
