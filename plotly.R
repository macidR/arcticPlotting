library(plotly)#install.packages('plotly')
library(PlotSvalbard) # https://github.com/MikkoVihtakari/PlotSvalbard#installation - purely for UTM coord conversion

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
max_depth <- 250 
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


rm(utm_coords)
#remove error high temps 
fdata <- fdata[fdata$tempC < 2,]
### plot! so easy wtf
p <- plot_ly(fdata, x = ~x, y = ~y, z = ~depth, color = ~tempC, name = ~name, showlegend = F) %>%
  add_markers() %>%
  layout( title = "WHOI ITP Buoy 105 Temperatures 10-250m",
          scene = list(xaxis = list(title = 'UTM x (lon)'),
                      yaxis = list(title = 'UTM y (lat)'),
                      zaxis = list(title = 'Depth (m)')),
         paper_bgcolor = '#f6fdff',
         plot_bgcolor = '#f6fdff')

p

