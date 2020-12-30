library(sp)
library(gstat)
library(rgdal)
mygrid<-function(x, y, z, buff, coords) {
  xyz <- as.data.frame(cbind(x,y,z), colnames=c("x","y","z"))
  pts <- SpatialPointsDataFrame(xyz[,c("x","y")], 
                                data= xyz,
                                proj4string = coords)
  ext<-bbox(pts)+t(matrix(buff,2,2)*c(-1,1))
  grd <- as.data.frame(spsample(pts, "regular", n=50000, bb=ext))
  names(grd)       <- c("X", "Y")
  coordinates(grd) <- c("X", "Y")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  # Add Wells_points's projection information to the empty grid
  proj4string(grd) <- proj4string(pts)
  #plot(grd)
  #points(pts)
  return(grd)
}

myidw<-function(x, y, z, grd, coords) {
  xyz <- as.data.frame(cbind(x,y,z), colnames=c("x","y","z"))
  pts <- SpatialPointsDataFrame(xyz[,c("x","y")], 
                                data= xyz,
                                proj4string = coords)
  # interpolation, idw
  idw <- gstat::idw(z ~ 1, pts, newdata=grd, idp=6)
  idw <-raster(idw)
  #plot(idw)
  return(idw)
}

mypts<-function(x, y, z, coords) {
  xyz <- as.data.frame(cbind(x,y,z), colnames=c("x","y","z"))
  pts <- SpatialPointsDataFrame(xyz[,c("x","y")], 
                                data= xyz,
                                proj4string = coords)
  return(pts)
}

mycontours<-function(r, levs) {
  cl<-rasterToContour(r, levels=levs)
  return(cl)
}

roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

x<-c(3030101,3030102,3030103)
y<-c(11291225,11291226,11291223)
z<-c(300,301,305)
coords<-CRS("+init=epsg:27200")  # make sure to comment whatever coordinate system this is
buff<-5 # feet to extend the raster beyond the points
r73<-mygrid(x,y,z,buff,coords)
PlotMap(r73)
points(pts)

Wells_points <- SpatialPointsDataFrame(df_all[,2:3], 
                                       data= df_all,
                                       proj4string = CRS("+init=ESRI:102700")) # montana state plane NAD83