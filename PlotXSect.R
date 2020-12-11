library(sp)
library(raster)
library(inlmisc)
library(plotKML)
library(grDevices)
#flux raster
x<-new_pts$coords.x1
y<-new_pts$coords.x2
z<-new_pts$`TCE Mass Flux (g/day)...16`
buff<-100
coords<-CRS("+init=EPSG:26934")
grd<-mygrid(x,y,z,buff,coords)
r3<-myidw(x,y,z,grd,coords)

plot(r3)
lines(fc)
points(new_pts)
text(new_pts, labels=new_pts$Well, data=new_pts, cex=0.6, font=2, adj=(c(1,-1)))

#GW raster
x<-new_pts$coords.x1
y<-new_pts$coords.x2
z<-new_pts$`MP Elevation (ft mllw)`-new_pts$`Stickup (ft)`-new_pts$`Groundwater Depth (ft bgs)`
r1<-myidw(x,y,z,grd,coords)
plot(r1)

#screen bottom raster
x<-new_pts$coords.x1
y<-new_pts$coords.x2
z<-new_pts$`MP Elevation (ft mllw)`-new_pts$`Stickup (ft)`-new_pts$`Screen Bottom (ft bgs)`
r2<-myidw(x,y,z,grd,coords)
plot(r2)

rs <- raster::stack(r1, r2, r3)
names(rs) <- c("r1", "r2", "r3")

#transect A
xy<-fc@lines[[1]]@Lines[[1]]  # change the first 1 in brackets to 2 for transect B, 3 for transect C
transect <- sp::Lines(list(sp::Line(xy)), ID = "Transect")
transect <- sp::SpatialLines(list(transect),
                             proj4string = raster::crs(rs))

unit <- "FEET"
explanation <- "TCE Mass Flux (grams/day)"
PlotCrossSection(transect, rs, 
                 geo.lays = c("r1", "r2"),
                 val.lays = "r3", 
                 #pal = palette(rainbow(3)),
                 ylab = "Elevation", 
                 asp = 3,
                 unit = unit, 
                 explanation = explanation,
                 features = new_pts, 
                 max.feature.dist = 130,
                 ylim = c(70, 150),
                 bg.col = "#E1E1E1", 
                 bend.label = "BEND IN\nSECTION",
                 scale.loc = NULL,
                 id=c("C","C'"))

#optional pallette work
pal<-SAGA_pal[2]
display.pal(pal, sel=1:length(pal), names=FALSE)
