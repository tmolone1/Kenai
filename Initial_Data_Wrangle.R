library(tidyverse)
library(readxl)
library(rgdal)
# path<-"M:/StoV/Tesoro/ProjectDocuments/KenaiRefinery/RCRA_Permit/QuarterlyMon/Reports/202101_Q21-1/SI_Appendix/SI_MassFlux.xlsx"
# colnames<-read_excel(path,
#                  sheet=2,
#                  range="A2:U2",
#                  col_names = FALSE)
# xcel<-read_excel(path,
#                  sheet=2,
#                  range="A4:U15",
#                  col_names = as.character(colnames))
# ##xcel<-read_excel(path,sheet=4)  # probably easier just to bring this in
# xcel<-xcel %>% filter(!grepl("Transect", Well)) %>% mutate (transect=(c(rep("A",3),rep("B",4),rep("C",3))))
# save(xcel, file="xcel.Rda")
load('xcel.Rda')

#fgdb<-"M:/StoV/Tesoro/GIS/Projects/TesoroKenai/Mapping/QuarterlyReports/202102_Q21-1/SI_MassFlux_Data.gdb"
fgdb<-'SI_MassFlux_Data.gdb'
#subset(ogrDrivers(), grepl("GDB", name))
#fc_list <- ogrListLayers(fgdb)
#print(fc_list)
fc <- readOGR(dsn=fgdb,layer="MF_Transects" )
pts<- readOGR(dsn=fgdb,layer="Q20_4_B_TCE_Wells")

newdf<-merge(xcel,pts,by=1,all.x=TRUE)
new_pts <- SpatialPointsDataFrame(newdf[,28:29], 
                                       data= newdf,
                                  proj4string = CRS("+init=EPSG:26934")) # Alaska State Plane Zone 4 NAD 83
plot(fc)
points(new_pts)
text(new_pts, labels=new_pts$Well, data=new_pts, cex=0.6, font=2, adj=(c(1,-1)))
newdf<-newdf %>% mutate(ground_elev = MPElev - Stickup)
write.csv(newdf, "collar.csv")
