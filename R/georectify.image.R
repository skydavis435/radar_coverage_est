library(raster)
library(rgdal)
library(MASS)
library(ggmap)
library(geosphere)
library(maptools)
library(ggthemes)

working.directory<-file.path(path.expand("~"),"Radar_Coverage")

data<-read.csv("ARTI_Data/ARTI_SeaTac_Olympic_2016.01.01_2016.12.31_NP_Fil.csv",header = T)

#georectify clutter image
clutter.map<-raster("Clutter/Tracker_Display_13.05.55_Tue_03Apr2018.bmp")
xmin(clutter.map) <- -122.365123
xmax(clutter.map) <- -122.266430
ymin(clutter.map) <- 47.418647
ymax(clutter.map) <- 47.485388
crs(clutter.map) <- "+proj=longlat +datum=WGS84"

#subset data to clutter map extents
sub.data <- subset(data, Longitude >= -122.365123 & Longitude <= -122.266430)
sub.data <- subset(sub.data, Latitude >= 47.418647 & Latitude <= 47.485388)
rm(data)

#Filter out extremely low clutter values
test<-clutter.map
test[test<15]<-NA

#Density calculation
density<-kde2d(x = sub.data$Longitude, y = sub.data$Latitude, n = c(128,128),lims = c(-122.365123,-122.266430,47.418647,47.485388))

#Convert clutter raster to polygons for use in ggmap
test.poly <- rasterToPolygons(test)

#Get a map
background<-get_map(location = c(-122.315776,47.451975),zoom = 13,maptype = "satellite")

p<-ggmap(background, extent = "normal", maprange = FALSE) %+% sub.data + aes(x = Longitude, y = Latitude)
p1 <- p+geom_polygon(data = test.poly,aes(x=long,y=lat,group=group),fill="grey",color="grey")
p2 <- p1 + geom_density2d(data = sub.data, aes(x = Longitude, y = Latitude),n=c(128,128),size=0.4, na.rm=T) #Plot the density contours
p3 <- p2 + stat_density2d(data = sub.data, aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),n=c(128,128), size = 0.01, bins = 16, geom = 'polygon',na.rm=T) #Add Transparent fill to density contours
p4 <- p3 + scale_fill_gradient(low = "yellow", high = "purple4") + scale_alpha(range = c(0.25, 0.75), guide = FALSE) #Color range and alpha levels to use for fill
p5 <- p4 + theme_map() + labs(title = "Seattle Tacoma International Airport",subtitle = "Clutter Map with 1 Year of Track Density", caption="Clutter Map: October 28, 2013\nTrack Data: January 1, 2016 - December 31, 2016 (no precip, no runways)")
ggsave(filename = "Clutter.and.TrackDensity.png",plot = p5,width = 15,height = 15,units = "in")
