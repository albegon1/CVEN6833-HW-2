# Sourcing libraries, data, and functions

##
## Source Libraries
##

libr=c("stats","coda","spBayes","geoR","fields","maptools","graticule","spatstat","raster")
options(warn=-1)
suppressPackageStartupMessages(lapply(libr, require,character.only = TRUE))

##
## Source files
##

jan=read.table("http://cires1.colorado.edu/~aslater/CVEN_6833/colo_monthly_precip_01.dat")
colnames(jan)<-c("Lat","Long","Elev","Prec")
jan<-jan[c("Prec",colnames(jan)[!colnames(jan) %in% "Prec"])] #Predictor first
jan$Prec<-ifelse(jan$Prec<=0,NA,jan$Prec) #Eliminate zero-neg values
jan<-jan[complete.cases(jan),] # Clean data

dem=read.table("http://cires1.colorado.edu/~aslater/CVEN_6833/colo_dem.dat")
colnames(dem)<-c("Lat","Long","Elev")

##
## Source functions
## 

### Spatial plotting
colo_pol<-map('county', 'colorado', fill = TRUE, col = palette(),plot=FALSE) # Colorado map
IDs <- sapply(strsplit(colo_pol$names, ":"), function(x) x[1]) # County names

colo_frame<-map('state', region = c('colorado', 'utah', 'wyoming','kansas','nebraska','oklahoma',
                                    'new mexico','arizona'),xlim=c(-109.3,-101.7),ylim=c(36.7,41.2),
                plot=FALSE,fill = TRUE,lforce = "e") # Sorrounding states
ID2s <- sapply(strsplit(colo_frame$names, ":"), function(x) x[1]) # Names

colo_pol <- map2SpatialPolygons(colo_pol, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
colo_frame<- map2SpatialPolygons(colo_frame,IDs=ID2s,proj4string=CRS("+proj=longlat +datum=WGS84"))

cit <- data.frame(Name=c('Denver','Ft Collins','Steamboat Sp','Winter Park','Colo Sprgs', # Col cities
                         'Grand Jct','Aspen','Pueblo','Lamar','Alamosa ','Trinidad'),
                  Lat=c(39.739235,40.58897,40.48549,39.89106,38.833881,39.065369,39.19067,
                        38.247685,38.081406,37.460484,37.167772),
                  Long=c(-104.99025,-105.082458,-106.83356,-105.76072,-104.821365,-108.569527,
                         -106.819199,-104.605081,-102.614833,-105.867995,-104.494041))
grat_sp <- graticule(lons = seq(-109,-102,by=1), lats = seq(37,41,by=1),
                     proj = "+init=epsg:3501")                              #add grid with lat and lon

# Transform data to PointSpatial Data
getSpatialDataFrame<-function(mydata){
  projdata<-SpatialPoints(data.frame(mydata$Long,mydata$Lat),
                          proj4string=CRS("+proj=longlat +datum=WGS84"))
  return(SpatialPointsDataFrame(projdata,data = mydata))
}
idata<-list(dem,jan,cit)
fdata<-list("dem_sp","jan_sp","cit_sp")
z<-lapply(idata,getSpatialDataFrame)
for(i in 1:length(z)){assign(as.character(fdata[i]),z[[i]])}

# Adopt NAD83 CRS
NAD83<-function(mapdata){
  return(spTransform(mapdata,CRS("+init=epsg:3501")))
}
idata<-list(dem_sp,jan_sp,cit_sp,colo_pol,colo_frame)
fdata<-list("dem_sp","jan_sp","cit_sp",'colo_pol','colo_frame')
z<-lapply(idata,NAD83)
for(i in 1:length(z)){assign(as.character(fdata[i]),z[[i]])}

# Coerce attibutes to {spatstat} from point data to point pattern (shapefile)
col_owin <- as.owin.SpatialPolygons(colo_pol) #smaller window
colf_owin<- as.owin.SpatialPolygons(colo_frame) #Colorado with other states
dem_ppp<-as.ppp(data.frame(dem_sp@coords,dem_sp@data$Elev), W=colf_owin)
jan_ppp<-as.ppp(data.frame(jan_sp@coords,jan_sp@data$Prec), W=colf_owin)
cit_ppp<-as.ppp(data.frame(cit_sp@coords,cit_sp@data$Name), W=colf_owin)


#plot function to include a new ppp with the Smooth.ppp function
generateplot<-function(ppp){
  plot(Smooth.ppp(ppp,eps=2000),col=rainbow(16),main = NULL)
  raster::lines(colo_pol,col="lightgray")
  points(X_sp,col="red",pch=".")
  points(cit_sp,pch=21)
  text(x = cit_sp@coords[,1], y = cit_sp@coords[,2], cit_sp@data$Name, pos = 4)
  plot(grat_sp,lty=2,add=TRUE)
  labs <- graticule_labels(lons = seq(-109,-102,by=1), lats = seq(37,41,by=1),
                           xline = -109, yline = 36.9,  proj = "+init=epsg:3501")
  text(labs, lab = parse(text = labs$lab), pos = c(2.7, 1.5)[labs$islon + 1], adj = 1.2)
}
