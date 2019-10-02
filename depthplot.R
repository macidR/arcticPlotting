library(viridis) # color
library(ggplot2)
library(PlotSvalbard) # https://github.com/MikkoVihtakari/PlotSvalbard#installation
### itp grddata to plot
# clean dir to start
wd <- 'arcticPlotting'
dir.create(wd)
setwd(wd)
rm(wd)

# list .dat files 
datlist <- list.files( pattern = "\\.dat$", recursive = T )
levels_dbar <- seq(10,760,2)
fdata <- NULL
for(i in 1:length(datlist)) {
  
  hdata <- read.delim2(datlist[i], header = F, sep = "", skip = 1, nrows = 1, stringsAsFactors= F )
  
  if(is.integer(hdata$V5)) {
    
    data <- read.delim2(datlist[i], sep = "", header = T, na.strings ="", stringsAsFactors= F, skip = 2, nrows = hdata[,5], fill = T)
    
    year <- hdata[,1]
    day <- as.numeric(hdata[,2])
    latitude <- as.numeric(hdata[,4])
    longitude <- as.numeric(hdata[,3])
    
    tdata <- data.frame(
      year,
      day,
      latitude,
      longitude,
      stringsAsFactors = F
    )
    for(ii in 1:length(levels_dbar)) {
      tempC <- as.numeric(data[(data$pressure.dbar. == levels_dbar[ii]),]$temperature.C.)
      tdata[,as.character(levels_dbar[ii])] <- tempC
    }
    fdata <- rbind(fdata,tdata)
    
    
  }
  if(i %in% seq(1,50000,10)) {
    print(paste0("DatNr: ",i)) 
  }
  
}  

# add xycoords to fdata
xycoords <- coord_lonlat_xy_transform(lon = fdata$longitude + 180, lat = fdata$latitude)
fdata$y <- xycoords$y
fdata$x <- xycoords$x
rm(xycoords)


# move to x,y positives for easier transformation
if(min(fdata$x) < 0) {
  fdata$x <- fdata$x-min(fdata$x)
}
fdata$x <- fdata$x + 1

if(min(fdata$y) < 0) {
  fdata$y <- fdata$y-min(fdata$y)
}
fdata$y <- fdata$y + 1

#plot(fdata$x, fdata$y)

# set x,y offsets for lower depths
offset_y <- (max(fdata$y) - min(fdata$y)) / 600

# 3d model into data3d data frame loop
data3d <- NULL
for(ii in 1:length(fdata)) {
  
  for(i in 1:90) {
    x <- fdata$x[ii] 
    y <- fdata$y[ii] - (offset_y*i)
    tempC <- fdata[ii,as.character(levels_dbar[i])]
    depth <- as.character(levels_dbar[i])
        
    data3dt <- data.frame(
      x,
      y,
      tempC,
      depth
    )
    data3d <- rbind(data3d,data3dt)
    
  }
  if(ii %in% seq(1,50000,1)) {
    print(paste0("fdata layer: ",ii)) 
  }
}

## make the plot, add layers from lower to top for visual pleasure
plot <- ggplot()
for(i in 90:1) {
  data3d_layer <- data3d[(data3d$depth == levels_dbar[i]),]
  nna <- which(!is.na(data3d_layer$tempC))
  plot <- plot + geom_path(data = data3d_layer[nna,], aes(x = x, y = y, color = tempC), lwd = .8, na.rm = T)
}
plot <- plot + scale_color_viridis(name = 'Temp (°C)') 
plot <- plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     panel.background = element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
                     axis.text.y=element_blank(),axis.ticks=element_blank(),
                     axis.title.x=element_blank(),axis.title.y=element_blank())

ggsave("itp110_200mx.png", plot = plot, width = 7, height = 5, dpi = 960)


# plot params
minT <- min(fdata[nna, as.character(levels_dbar[i])])
maxT <- max(fdata[nna, as.character(levels_dbar[i])])
my_breaks <- seq(minT,maxT,by = ((maxT - minT)/7))
i <- 10
min_lat <- 71 # degrees N, for plot & data filter
utm_coords <- transform_coord(lon = as.numeric(fdata$longitude), lat = as.numeric(fdata$latitude), new.names = c("lon.utm", "lat.utm"), 
                              proj.og = "+proj=longlat +datum=WGS84", 
                              proj.out = "+proj=stere +lat_0=90 +lat_ts=0 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", 
                              map.type = 'panarctic', 
                              verbose = FALSE, 
                              bind = FALSE)

nna <- which(!is.na(fdata[as.character(levels_dbar[i])]))
# plot 
basemap("panarctic", bathymetry = TRUE, limits = min_lat, bathy.style = "poly_greys") +
  geom_point(data = utm_coords, aes(x = lon.utm, y = lat.utm), size = 1.5, colour = "black" )
ggsave(paste0(sprintf("%03.0f",levels_dbar[i]),".png"), width = 7, height = 5, dpi = 240)
