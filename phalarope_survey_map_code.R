##phalarope report code
##m tarjan
##prepared January 22, 2021

##make a map of phalarope sites surveyed
library(rgdal)
library(rgeos) ##required for gintersection
library(maptools) ##required for fortify
land<- rgdal::readOGR(dsn = "S:/Science/GIS/CA_map_layers", layer= "north_america")
#CA<-rgdal::readOGR(dsn = "S:/Science/GIS/Salt Pond", layer= "CA_boundary_NAD83")
ponds.poly<-rgdal::readOGR(dsn = "S:/Science/GIS/Salt Pond/all_salt_pond_grids_2014", layer= "2014_11_19_pond_scale")

##check projections
proj4string(ponds.poly); proj4string(land)
land.utm <- spTransform(land, CRS("+init=epsg:3157")) # reproject
proj4string(land.utm)

##example
#https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf

##clip land by pond bounding box
##functions from https://gis.stackexchange.com/questions/46954/clip-spatial-object-to-bounding-box-in-r
as.SpatialPolygons.bbox <- function( bbox, proj4stringFrom=CRS("+proj=longlat +datum=WGS84"), proj4stringTo=NULL ) {
  # Create unprojected bbox as spatial object
  bboxMat <- rbind( c(bbox['x','min'],bbox['y','min']), c(bbox['x','min'],bbox['y','max']), c(bbox['x','max'],bbox['y','max']), c(bbox['x','max'],bbox['y','min']), c(bbox['x','min'],bbox['y','min']) ) # clockwise, 5 points to close it
  bboxSP <- SpatialPolygons( list(Polygons(list(Polygon(bboxMat)),"bbox")), proj4string=proj4stringFrom  )
  if(!is.null(proj4stringTo)) {
    bboxSP <- spTransform( bboxSP, proj4stringTo )
  }
  bboxSP
}

#box.expand<-cbind(summary(ponds.poly)$bbox[,1]*0.9995, summary(ponds.poly)$bbox[,2]*1.0005); colnames(box.expand)<-c("min", "max")
box.expand<-summary(ponds.poly)$bbox*cbind(c(0.99, 0.9995), c(1.0009, 1.0005))

box<-as.SpatialPolygons.bbox(bbox = box.expand, proj4stringFrom = CRS(proj4string(ponds.poly)), proj4stringTo = CRS(proj4string(land.utm)))
if (exists("land.clip")==F) {
  land.clip<-gIntersection(spgeom1 = land.utm, spgeom2=box)
}

ponds.poly@data$Pond<-gsub(pattern = "N4Aa", replacement = "N4AA", x = ponds.poly@data$Pond)
ponds.poly@data$Pond<-gsub(pattern = "N4Ab", replacement = "N4AB", x = ponds.poly@data$Pond)
ponds.poly@data$Pond<-gsub(pattern = "R5S", replacement = "RS5", x = ponds.poly@data$Pond)


ponds.poly.df<-ponds.poly
ponds.poly.df@data$id <- rownames(ponds.poly.df@data)
ponds.f <- ggplot2::fortify(ponds.poly.df, region = "id")
ponds.df <- plyr::join(ponds.f, ponds.poly.df@data, by = "id")

##map of sites to survey
sites.poly<-rgdal::readOGR(dsn = "S:/Science/Waterbird/Program Folders (Gulls, SNPL, ADPP, etc)/Cargill Pond Surveys/PHAL survey/PHAL data", layer= "Phalarope survey sites")
sites.poly <- spTransform(sites.poly, CRS(proj4string(ponds.poly))) # reproject
sites.poly<-sites.poly[sites.poly@data$Site.Name != "Crittenden Marsh East" & sites.poly@data$Site.Name != "Spreckles Marsh",]

##add site type to both sets of sites (SBSPRP and eBird)
ponds.poly@data$type<-"SBSPRP"
sites.poly@data$type<-"eBird"

##define survey.sites among the ponds
phal.counts<-gdata::read.xls("S:/Science/Waterbird/Program Folders (Gulls, SNPL, ADPP, etc)/Cargill Pond Surveys/PHAL survey/PHAL data/Phalarope Data all years.xlsx", sheet="Phalarope Data")
phal.counts<-phal.counts[which(str_detect(phal.counts$Date, pattern = "2020")),]
survey.sites<-unique(phal.counts$Site.name)

all.sites.poly<-ponds.poly[ponds.poly@data$Pond %in% survey.sites,] ##subset to phal sites in sbsprp
all.sites.poly@data<-subset(all.sites.poly@data, select=c(Pond, Area, type)) ##subset the data
colnames(all.sites.poly@data)<-c("Site.Name", "Area", "type") ##rename columns to match other phal sites
sites.poly@data<-subset(sites.poly@data, select=c(Site.Name, Area, type)) ##subset data for other phal sites
##merge two shapefiles for one phal site shapefile
all.sites.poly<-rbind(all.sites.poly, sites.poly)

##add centroid for adding labels
all.sites.poly@data<-cbind(all.sites.poly@data, as.data.frame(coordinates(all.sites.poly)))

##make the map
##legend
legend.text<-data.frame(id=1:nrow(all.sites.poly@data), equal="=",site=all.sites.poly@data$Site.Name)
legend.text<-tidyr::unite(data = legend.text, col="legend", sep=" ")
#legend.text<-data.frame(Site=all.sites.poly@data$Site.Name)

library(gridExtra)
mytheme <- gridExtra::ttheme_default(
  core = list(fg_params=list(cex = 0.75)),
  colhead = list(fg_params=list(cex = 0.5)),
  rowhead = list(fg_params=list(cex = 0.5)))

map <- ggplot(data= all.sites.poly@data) + coord_equal()
map <- map + geom_polygon(aes(long, lat, group=group), data=land.clip, fill="light grey")
map <- map + geom_polygon(data= ponds.df, aes(long, lat, group=group), color="black", fill="grey")
map <- map + geom_polygon(data= all.sites.poly, aes(long, lat, group=group), color="red", fill="grey", size=1.25)
map <- map + scale_x_continuous(name = "UTM E-W (m)", limits = c(summary(box)$bbox[1,1]-1, summary(box)$bbox[1,2]+1), expand = c(0,0))
map <- map + scale_y_continuous(name = "UTM N-S (m)", limits = c(summary(box)$bbox[2,1]-1, summary(box)$bbox[2,2]+1), expand = c(0,0))
map <- map + theme_classic() #+ theme(panel.background = element_rect(fill= "grey"))
map <- map + geom_path(aes(long, lat), data = box)
map <- map + annotate("text", label = "South\n San Francisco\n Bay", x = 571500, y = 4160000, size = 4, colour = "dark blue")
#map <- map + theme(axis.text.x=element_blank(), axis.text.y = element_blank())
#map <- map + theme(axis.ticks=element_blank())
map <- map + geom_text(aes(label = 1:nrow(all.sites.poly@data), x = V1, y = V2, fontface="bold"))
map <- map + annotation_custom(gridExtra::tableGrob(legend.text[1:6,], theme=mytheme), xmin=579000, xmax=590700, ymin=4150000, ymax=4167000)
map <- map + annotation_custom(gridExtra::tableGrob(legend.text[7:12,], theme=mytheme), xmin=584700, xmax=590700, ymin=4150000, ymax=4167000)
#map <- map + annotation_custom(gridExtra::tableGrob(legend.text[(length(survey.sites)/3*2+1):(length(survey.sites)/3*3),], theme=mytheme), xmin=591100, xmax=591200, ymin=4150000, ymax=4167000)
map <- map + annotation_custom(gridExtra::tableGrob(legend.text[13:nrow(legend.text),], theme=mytheme), xmin=569000, xmax=576000, ymin=4140000, ymax=4149000)
map

png(filename = str_c("PHALsurveysites2020.png"), units="in", width=6.5, height=7,  res=400);print(map); dev.off()
