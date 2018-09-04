coverage.csv_v2<-function(az=seq(0,360,2.8125),el=seq(0,70,8.75),max.dist = 9656.06,sensor.type = "radar"){
  
  #NOTE: ADJUST UTM ZONE IN LINE 
  
  #Require
    require(geosphere)
    require(data.table)
    require(bit64)
  #Variables
    working.directory<-file.path(path.expand("~"),"Radar_Coverage")
    radar<-c(-97.053827,32.875790)
  #Math...loop. I'm sorry R community
    for(i in 1:length(el)){
      data<-data.frame(az)
      data$el<-el[i]
      data$slant.range<-cos(el[i]*(pi/180))*max.dist
      coords<-destPointRhumb(p = radar,b = data$az,d = data$slant.range)
      data$lon<-coords[,1]
      data$lat<-coords[,2]
      data$z<-sin(el[i]*(pi/180))*max.dist
      assign("data",data,.GlobalEnv)
      filename<-paste0(el[i],"_.csv")
      write.csv(x = data,file = file.path(working.directory,"Temp",filename),row.names = F)
    }
  #Combine all the elevation angles
    setwd(file.path(working.directory,"Temp"))
    filelist<-list.files(pattern="\\.csv$")
    datalist<-lapply(filelist,fread)
    complete.coverage<-rbindlist(datalist)
    complete.coverage<-complete.coverage[order(el),]
    dont.forget.the.radar<-data.frame(NA,NA,NA,radar[1],radar[2],0)
    colnames(dont.forget.the.radar)<-c("az","el","slant.range","lon","lat","z")
    complete.coverage<-rbind(complete.coverage,dont.forget.the.radar)
  
    #Add UTM Coords (ADJUST UTM ZONE IF NECESSARY!)
      lonlat<- SpatialPoints(cbind(complete.coverage$lon,complete.coverage$lat), proj4string=CRS("+proj=longlat +datum=WGS84"))
      utm<- spTransform(lonlat,CRS("+proj=utm +zone=14S"))
      complete.coverage$Easting<-utm$coords.x1
      complete.coverage$Northing<-utm$coords.x2
      assign("complete.coverage",complete.coverage,.GlobalEnv)
    
    #Save as CSV
      filename.1<-paste0(sensor.type,"_complete.coverage.csv")
      write.csv(x = complete.coverage,file = file.path(working.directory,"Result",filename.1),row.names = F)
      
    #Make 1/00 scale model for blender, centered at (0,0,0)
      blend<-data.frame(complete.coverage$Easting,complete.coverage$Northing,complete.coverage$z)
      colnames(blend)<-c("X","Y","Z")
      #Transpose
        blend$X<-blend$X - blend$X[length(blend$X)]
        blend$Y<-blend$Y - blend$Y[length(blend$Y)]
      #Scale
        blend$X<-blend$X/100
        blend$Y<-blend$Y/100
        blend$Z<-blend$Z/100
    #Save
      filename.2<-paste0(sensor.type,"_complete.coverage.blend.csv")
      write.csv(x = blend,file = file.path(working.directory,"Result",filename.2),row.names = F)
    
    
    file.remove(filelist)
}