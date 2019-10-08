# fed by WHOI ITP grddatas
# plots temperature profiles in 3d scatterplot, can be published as WebGL
# - added bathy layer at the bottom, for now depth indicated by height. colors a problem
# need all below libraries, install if necessary
# 
# params to set: max_depth, title in plot (all the way below)
# probably have to fix bathy params, tested only for itp 119 
#
# macid 
#
library(plotly)# install.packages('plotly')
library(PlotSvalbard) # https://github.com/MikkoVihtakari/PlotSvalbard#installation - purely for UTM coord conversion
library(marmap) # install.packages('marmap') - request NOAA bathy data for small areas
library(reshape2) # install.packages('reshape2') -- uh oh need to learn to use melt better
# clean dir to start
#wd <- 'arcticPlotting'
#dir.create(wd)
#setwd(wd)
#rm(wd)
#files_to_download <- c("itp116grddata.zip", "itp114grddata.zip", "itp104grddata.zip", "itp105grddata.zip", "itp113grddata.zip", "itp119grddata.zip", "itp118grddata.zip", "itp117grddata.zip" )

# put dat files you want to load somewhere in a (sub) folder of the working dir
datlist <- list.files( pattern = "\\.dat$", recursive = T )

# init / reset data frame
fdata <- NULL

# set max dbars to read 
max_depth <- 220 
# set if you want to skip some levels per dat file to keep the data points lower, enable 'ENABLE SKIP' line in loop below
#skip_nr <- 4

# loop through dats
for(i in 1:length(datlist)) {
  
  # get .dat header info
  hdata <- read.delim2(datlist[i], header = F, sep = "", skip = 1, nrows = 1, stringsAsFactors= F )
  
  # if there's oberservation, else skip
  if(is.integer(hdata$V5)) {
    # init / reset temp data frame for dat file
    tdata <- NULL
    # read dat data into temp table
    txdata <- read.delim2(datlist[i], sep = "", header = T, na.strings ="", stringsAsFactors= F, skip = 2, nrows = hdata[,5], fill = T)
    
    # check if there's observations with less than max depth, else skip
    if(is.integer(length(txdata[txdata$pressure.dbar. <= max_depth,]$temperature.C.))){
      # select observations with lower depth than max depth
      txdata <- txdata[txdata$pressure.dbar. <= max_depth,]
      #ENABLE SKIP#
      # if(length(txdata$temperature.C.) > 1){txdata <- txdata[seq(1,length(txdata$temperature.C.),by = skip_nr),]}
      # copy temp to temp data frame
      tdata$tempC <- as.numeric(txdata$temperature.C.)
      # copy - depth (dbar) to create depth (m)
      tdata$depth <- -as.numeric(txdata$pressure.dbar.)
      # add lat/lon, repeat per observation
      tdata$lat <- rep(as.numeric(hdata[,4]), length(tdata$tempC))
      tdata$lon <- rep(as.numeric(hdata[,3]), length(tdata$tempC))
      # add name for data points, plotly user <br> for newline
      tdata$name <- rep( paste( paste("lon:",hdata[,3]),
                                paste("lat:",hdata[,4]),
                                paste("Year:",hdata[,1]),
                                paste("Day:",hdata[,2]), sep = "<br>"), 
                        length(tdata$tempC))
      # add to final df
      fdata <- merge(fdata,tdata, all = T)
    }
    
  }
  # tracking progress
  if(i %in% seq(1,50000,10)) {
    print(paste0("DatNr: ",i,"/",length(datlist))) 
  }
  
}  
#cleanup
rm(hdata,tdata,txdata,datlist)

# create UTM coords, results in meters measurement
utm_coords <- transform_coord(lon = fdata$lon, lat = fdata$lat, new.names = c("lon.utm", "lat.utm"), 
                              proj.og = "+proj=longlat +datum=WGS84", 
                              proj.out = "+proj=stere +lat_0=90 +lat_ts=0 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", 
                              map.type = 'panarctic', 
                              verbose = FALSE, 
                              bind = FALSE)
#add to df
fdata$x <- utm_coords$lon.utm
fdata$y <- utm_coords$lat.utm

# get bathy params for area
range_lon <- max(fdata$lon) - min(fdata$lon)
range_lat <- max(fdata$lat) - min(fdata$lat)
if(range_lon > range_lat) {
  lon1 <- min(fdata$lon)
  lon2 <- max(fdata$lon)
  range_ll_margin <- (range_lon - range_lat) / 2
  lat1 <- min(fdata$lat) - range_ll_margin
  lat2 <- max(fdata$lat) + range_ll_margin
} else {
  lat1 <- min(fdata$lat)
  lat2 <- max(fdata$lat)
  range_ll_margin <- (range_lat - range_lon) / 2
  lon1 <- min(fdata$lon) - range_ll_margin
  lon2 <- max(fdata$lon) + range_ll_margin
}

# get bathy data for area.. R so powerful one line bam:
bathy_data <- melt(unclass( getNOAA.bathy(lon1-3, lon2+3, lat1, lat2, resolution = 5)))

# funky stuff to make fit, will clean up once I figure out how to handle seperate color scale for this data, round to 10meters, put in 1-n range for z axis scaling
bathy_data$z <- round(bathy_data$value / 10,0)
bathy_data$z <- bathy_data$z - min(bathy_data$z) + 1
# put highest bathy point below max depth
bathy_data$z <- bathy_data$z - (max_depth + max(bathy_data$z))
# create bathy UTM coords and replace lat lons
utm_coords <- transform_coord(lon = bathy_data$Var1, lat = bathy_data$Var2, new.names = c("lon.utm", "lat.utm"), 
                              proj.og = "+proj=longlat +datum=WGS84", 
                              proj.out = "+proj=stere +lat_0=90 +lat_ts=0 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", 
                              map.type = 'panarctic', 
                              verbose = FALSE, 
                              bind = FALSE)
#add bathy xy to df and remove lat/lon
bathy_data$x <- utm_coords$lon.utm
bathy_data$y <- utm_coords$lat.utm
bathy_data$Var1 <- NULL
bathy_data$Var2 <- NULL

# setup square range for data projection
range_x <- max(fdata$x) - min(fdata$x)
range_y <- max(fdata$y) - min(fdata$y)
margin <- (range_x + range_y) / 20
if(range_x > range_y) {
  range_cx <- c(min(fdata$x) - margin, max(fdata$x) + margin)
  range_xy_margin <- (range_x - range_y) / 2
  range_cy <- c(min(fdata$y) - range_xy_margin - margin, max(fdata$y) + range_xy_margin  + margin) 
} else {
  range_cy <- c(min(fdata$y) - margin,max(fdata$y) + margin) 
  range_xy_margin <- (range_x - range_y) / 2 
  range_cx <- c(min(fdata$x) - range_xy_margin - margin, max(fdata$x) + range_xy_margin  + margin) 
}




rm(utm_coords, lat1,lat2,lon1,lon2,range_lat,range_ll_margin,range_lon, range_x, range_y, range_xy_margin, margin)


#remove error high temps 
fdata <- fdata[fdata$tempC < 2,]

### plot! so easy wtf
p <- plot_ly() %>% 
  add_markers(data = fdata, x = ~x, y = ~y, z = ~depth, color = ~tempC, name = ~name, showlegend = F) %>%
  #add_markers(data = fdata, x = ~x, y = ~y, z = ~depth, color = ~tempC, name = ~name, showlegend = F) %>% 
  add_markers(inherit = F, data = bathy_data, x = ~x, y = ~y, z = ~z, marker = list( 
              color = "#C3C3C3", name = "")) %>%
  layout( title = "WHOI ITP Buoy 119 Temperatures 6-220m + Bathymetry",
          scene = list(xaxis = list(title = 'UTM x (lon)', range = range_cx),
                      yaxis = list(title = 'UTM y (lat)', range = range_cy),
                      zaxis = list(title = 'Depth (m)')),
         paper_bgcolor = '#f6fdff',
         plot_bgcolor = '#f6fdff')

p

