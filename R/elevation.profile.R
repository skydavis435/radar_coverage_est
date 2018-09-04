library(rgbif)
library(geosphere)
library(ggplot2)
library(ggthemes)
library(ggmap)
library(cowplot)

working.directory<-file.path(path.expand("~"),"Radar_Coverage")
api.key<-<enter key here>
olympic.lon.lat<-c(-122.315776,47.451975)

#Read in what you want to calculate. Distances and bearings needed to start
data<-read.csv(file = file.path(working.directory,"Elevation_Profiles","sea_east_west.csv"),header = T)

#Calculate lon.lat of points along line
lon.lat<-destPoint(p = olympic.lon.lat,b = data$Bearing,d = data$Distance..m.)

#add to data.frame
data$lon<-lon.lat[,1]
data$lat<-lon.lat[,2]

#Obtain lon/lat elevation from Google... mind the API limits
elev<-elevation(longitude = data$lon,latitude = data$lat,key = api.key)

#Add elevation to data.frame
data$elev.m<-elev$elevation

#West is negative today
#test<-data
data$Distance..m.<-ifelse(data$Bearing == 270,-1*data$Distance..m.,data$Distance..m.)

#beam calculations
radar.elev<-subset(data, Distance..m. == 0)
data$Distance..m.beam<-ifelse(data$Distance..m. < 0, data$Distance..m. *-1,data$Distance..m.)
data$beam.bottom.m<-tan(-11*pi/180)*data$Distance..m.beam + radar.elev$elev.m
data$beam.top.m<-tan(11*pi/180)*data$Distance..m.beam + radar.elev$elev.m

data$beam.top.ft<-data$beam.top.m * 3.28084
data$beam.bottom..ft<-radar.elev$elev.m * 3.28084 # even with horizon, even if it is supposed to be -11 degrees
data$Distance..mi.<-data$Distance..m. * 0.000621371
data$ground.elev.ft<-data$elev.m * 3.28084

#Save it
write.csv(x = data,file = file.path(working.directory,"Elev_Profile_Result","Olympic_East_West_6k.csv"),row.names = F)


#plot it
p<-ggplot(data = data,aes(x=Distance..mi.,y=ground.elev.ft))+geom_line(color="brown") + geom_line(data = data,aes(x=Distance..mi.,y=beam.top.ft),color="blue") + geom_line(data = data,aes(x=Distance..mi.,y=beam.bottom..ft),color="orange",linetype=2)
final <- p + theme_gdocs() + ggtitle(label = "Seattle Tacoma International Airport: Olympic Radar Beam Coverage",subtitle = "West to East -- extending 1.86 miles from Olympic Radar in either direction.") + ylab("Elevation (ft above sea level)") + xlab("Distance from Radar (mi)")
ggsave(filename = "TEST_SEA_Olympic_EastWest_coverage.png",plot = final,width = 10,height = 10,units = "in")


