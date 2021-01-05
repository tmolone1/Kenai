library(sp)
library(raster)
library(inlmisc)
library(plotKML)
library(grDevices)
library(tidyverse)
years<-c(2015,2017,2020)
constituents<-c("TCE Mass Flux","TCE and Daughter Products Molar Flux","Benzene Mass Flux")
explanations<-c("TCE Mass Flux (g/d)","TCE and Daughters Molar Flux (mole/day)","Benzene Mass Flux (g/d)")

for(i in years) {
  for(j in 1:3) {
#change all these to change the plots
year<-i # any of the following:   2020    2017     2015
constituent<-constituents[j]   # any of the following: "TCE Mass Flux"   "TCE and Daughter Products Molar Flux"   "Benzene Mass Flux"
explanation <- explanations[j]  # any of the following: "TCE Mass Flux (g/d)"  "TCE and Daughters Molar Flux (mole/day)"  "Benzene Mass Flux (g/d)"


#dont mess with these
yearmin=paste0(year,"-01-01")
yearmax=paste0(year,"-12-31")
dat1<- tibble(new_pts@data)
dat1<- dat1 %>% filter(`Sample Date` >= yearmin, `Sample Date` <= yearmax) %>% dplyr::select(Well, constituent, coords.x1, coords.x2)
const_rng <- tibble(new_pts@data) %>% dplyr::select(constituent) %>% range()
x<-as.numeric(new_pts$coords.x1)
y<-as.numeric(new_pts$coords.x2)
z<-as.numeric(new_pts$MPElev)
buff<-100
coords<-CRS("+init=EPSG:26934")
grd<-mygrid(x,y,z,buff,coords)
feats<-new_pts[new_pts$`Sample Date` >= yearmin & new_pts$`Sample Date` <= yearmax,]

#flux raster
x<-as.numeric(dat1$coords.x1)
y<-as.numeric(dat1$coords.x2)
z<-as.numeric(unlist(dat1 %>% dplyr::select(constituent)))
r3<-myidw(x,y,z,grd,coords)

# plot(r3)
# lines(fc)
# points(new_pts)
# text(new_pts, labels=new_pts$Well, data=new_pts, cex=0.6, font=2, adj=(c(1,-1)))

#GW raster
x<-new_pts$coords.x1
y<-new_pts$coords.x2
z<-rep(54.37, length(x))
r1<-(r3/r3)*88.02
# plot(r1)

#screen bottom raster
x<-new_pts$coords.x1
y<-new_pts$coords.x2
z<-rep(74, length(x))
r2<-(r3/r3)*68.39
# plot(r2)
r0<-(r3/r3)*142.39

rs <- raster::stack(r0, r1, r2, r3)
names(rs) <- c("r0", "r1", "r2", "r3")



#transect A
xy<-fc@lines[[1]]@Lines[[1]]  # change the first 1 in brackets to 2 for transect B, 3 for transect C
transect <- sp::Lines(list(sp::Line(xy)), ID = "Transect")
transect <- sp::SpatialLines(list(transect),
                             proj4string = raster::crs(rs))
breaks<-seq(floor(min(const_rng)),roundUpNice(max(const_rng)), roundUpNice((diff(const_rng))/12))



dev.new()
unit <- "FEET"
plot_A <- PlotCrossSection(transect, rs, 
                 geo.lays = c("r1", "r2"),
                 val.lays = c("r3"),
                 wt.lay = "r0",
                 pal = GetColors(scheme = "vik"),
                 ylab = "ELEVATION", 
                 asp = 2,
                 unit = unit, 
                 breaks = breaks,
                 explanation = explanation,
                 features = feats, 
                 max.feature.dist = 150,
                 ylim = c(50, 150),
                 #bg.col = "#E1E1E1", 
                 wt.col = "orange4",
                 bend.label = "BEND IN\nSECTION",
                 scale.loc = NULL,
                 id=c("A","A'"),
                 file = paste0("images/",constituent," ",year,"TransectA.png"))
graphics.off()


#transect B 
r1<-(r3/r3)*86.24
r2<-(r3/r3)*68.08
r0<-(r3/r3)*141.33
rs <- raster::stack(r0, r1, r2, r3)
names(rs) <- c("r0", "r1", "r2", "r3")
xy<-fc@lines[[2]]@Lines[[1]]  # change the first 1 in brackets to 2 for transect B, 3 for transect C
transect <- sp::Lines(list(sp::Line(xy)), ID = "Transect")
transect <- sp::SpatialLines(list(transect),
                             proj4string = raster::crs(rs))


dev.new()
plot_B<- PlotCrossSection(transect, rs, 
                 geo.lays = c("r1", "r2"),
                 val.lays = c("r3"),
                 wt.lay = "r0",
                 pal = GetColors(scheme = "vik"),
                 ylab = "ELEVATION", 
                 asp = 2,
                 unit = unit, 
                 breaks = breaks,
                 explanation = explanation,
                 features = feats, 
                 max.feature.dist = 80,
                 ylim = c(50, 150),
                 #bg.col = "#E1E1E1", 
                 wt.col = "orange4",
                 bend.label = "BEND IN\nSECTION",
                 scale.loc = NULL,
                 id=c("B","B'"),
                 file = paste0("images/",constituent," ",year,"TransectB.png"))

graphics.off()


#transect C 
r1<-(r3/r3)*87.33
r2<-(r3/r3)*71.76
r0<-(r3/r3)*140.26
rs <- raster::stack(r0, r1, r2, r3)
names(rs) <- c("r0", "r1", "r2", "r3")
xy<-fc@lines[[3]]@Lines[[1]]  # change the first 1 in brackets to 2 for transect B, 3 for transect C
transect <- sp::Lines(list(sp::Line(xy)), ID = "Transect")
transect <- sp::SpatialLines(list(transect),
                             proj4string = raster::crs(rs))

dev.new()
PlotCrossSection(transect, rs, 
                 geo.lays = c("r1", "r2"),
                 val.lays = c("r3"),
                 wt.lay = "r0",
                 pal = GetColors(scheme = "vik"),
                 ylab = "ELEVATION", 
                 asp = 2,
                 unit = unit, 
                 breaks = breaks,
                 explanation = explanation,
                 features = feats, 
                 max.feature.dist = 130,
                 ylim = c(50, 150),
                 #bg.col = "#E1E1E1", 
                 wt.col = "orange4",
                 bend.label = "BEND IN\nSECTION",
                 scale.loc = "bottomright",
                 id=c("C","C'"),
                 file = paste0("images/",constituent," ",year,"TransectC.png"))
graphics.off()



# rmarkdown::render("test.Rmd", output_file = "Benzene2020.pdf")
rmarkdown::render("test.Rmd", output_file = paste0(constituent," ",year,".pdf"))
# rmarkdown::render("test.Rmd", output_file = "Benzene2015.pdf")

# 
# 
# #optional pallette work
# pal<-SAGA_pal[2]
# display.pal(pal, sel=1:length(pal), names=FALSE)
# 
# op <- par(mfrow = c(7, 1), omi = c(1, 1, 1, 1),
#           mar = c(2, 3, 2, 3))
# AddColorKey(breaks = 0:10,
#             explanation = "Example description of data variable.")
# AddColorKey(breaks = 0:1000, at = pretty(0:1000))
# AddColorKey(breaks = c(0, 1, 2, 4, 8, 16))
# breaks <- c(pi * 10^(-5:5))
# AddColorKey(breaks = breaks, log = TRUE)
# AddColorKey(breaks = breaks,
#             at = breaks[as.logical(seq_along(breaks) %% 2)],
#             scipen = NULL, log = TRUE)
# AddColorKey(is.categorical = TRUE, labels = LETTERS[1:5])
# AddColorKey(is.categorical = TRUE,
#             col = GetColors(5, scheme = "bright"))
# par(op)
# ?AddColorKey
# ?AddColorKey
  }
}

  for(j in 1:3) {
    constituent<-constituents[j]   # any of the following: "TCE Mass Flux"   "TCE and Daughter Products Molar Flux"   "Benzene Mass Flux"

    rmarkdown::render("test2.Rmd", output_file = paste0(constituent,".pdf"))
    
  }
