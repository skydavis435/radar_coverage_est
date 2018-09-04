coverage<-function(radar.lon = -97.053827,radar.lat = 32.875790, max.dist = 9656.06){
  #Required Packages
    require(sp)
    require(geosphere)
    require(plotKML)
  #Variables
    radar<-c(radar.lon, radar.lat)
    az<-seq(0,360,2.8125)
    el<-seq(0,70,8.75)
  #Calculations
    data<-data.frame(az)
    data$center.lon<-radar[1]
    data$center.lat<-radar[2]
    coords<-destPointRhumb(p = radar,b = data$az,d = max.dist)
    data$end.lon<-coords[,1]
    data$end.lat<-coords[,2]
    assign("data",data,.GlobalEnv)
  #Make a KML (needs work)
    pol1<-data[1:2,]
    crds <- cbind(pol1$end.lon,pol1$end.lat)
    test <- cbind(data$center.lon, data$center.lat)
    crds <- rbind(crds,radar)
    Pl <- Polygon(crds)
    ID <- "I don’t know what I’m doing...yet"
    Pls <- Polygons(list(Pl), ID=ID)
    SPls <- SpatialPolygons(list(Pls),proj4string = CRS("+proj=longlat +datum=WGS84"))
    df <- data.frame(value=1, row.names=ID)
    SPDF <- SpatialPolygonsDataFrame(SPls, df)
    plotKML(SPDF)
  
    pol2<-data[3:4,]
    crds2 <- cbind(pol2$end.lon,pol2$end.lat,50)
    crds2 <- rbind(crds2,radar)
    Pl2 <- Polygon(crds2)
    ID2 <- "I don’t know what I’m doing...yet_2"
    Pls2 <- Polygons(list(Pl2), ID=ID2)
    SPls2 <- SpatialPolygons(list(Pls2),proj4string = CRS("+proj=longlat +datum=WGS84"))
    df2 <- data.frame(value=1, row.names=ID2)
    SPDF2 <- SpatialPolygonsDataFrame(SPls2, df2)
    wtf<-rbind(SPDF,SPDF2)
    plotKML(wtf)
}