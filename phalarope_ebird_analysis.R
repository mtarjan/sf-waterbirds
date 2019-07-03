##phalarope survey plan
##M Tarjan
##June 18, 2019

##a package that processes ebird data: http://strimas.com/ebird-best-practices/ebird.html#ebird-extract

library(ggplot2)
library(stringr)

#install.packages("auk")
library(auk)
#auk::auk_set_ebd_path("S:/Science/Waterbird/Program Folders (Gulls, SNPL, ADPP, etc)/Cargill Pond Surveys/Reports/2019 Waterbird Trend Assessment/sf-waterbirds/phalarope_ebird_data")

#ebd<-auk_ebd(file = "ebd_US-CA-001_renpha_201501_201906_relMay-2019.txt")
##define filters
#ebd_filters <- ebd %>% 
  #auk_species("Wood Thrush") %>% 
  ## southeastern coastal plain bcr
  #auk_bcr(bcr = 27) %>% 
  ## june, use * to get data from any year
  #auk_date(date = c("*-06-01", "*-06-30")) %>% 
  ## restrict to the standard traveling and stationary count protocols
  ##auk_protocol(protocol = c("Stationary", "Traveling")) %>% 
#  auk_complete()
#ebd_filters

##skip pervious code and just read in pre-subsetted file
#data<-read_ebd("S:/Science/Waterbird/Program Folders (Gulls, SNPL, ADPP, etc)/Cargill Pond Surveys/Reports/2019 Waterbird Trend Assessment/sf-waterbirds/phalarope_ebird_data/ebd_US-CA-001_renpha_201501_201906_relMay-2019.txt")
#data2<-read_ebd("S:/Science/Waterbird/Program Folders (Gulls, SNPL, ADPP, etc)/Cargill Pond Surveys/Reports/2019 Waterbird Trend Assessment/sf-waterbirds/phalarope_ebird_data/location2/ebd_US-CA-085_renpha_201501_201906_relMay-2019.txt")

#data<-rbind(data.frame(data), data.frame(data2))

data.folders<-"S:/Science/Waterbird/Program Folders (Gulls, SNPL, ADPP, etc)/Cargill Pond Surveys/Reports/2019 Waterbird Trend Assessment/sf-waterbirds/phalarope_ebird_data/"
data<-dim(0)
for (j in 1:length(list.files(data.folders))) {
  data.folder<-str_c(data.folders, list.files(data.folders)[j], "/")
  file.temp<-list.files(data.folder)[which(str_detect(string = list.files(data.folder), pattern = "ebd"))]
  data.temp<-read_ebd(str_c(data.folder, file.temp))
  data<-rbind(data, data.frame(data.temp))
}


##plot number of birds observed over time
data.plot<-subset(data, format(observation_date, "%m") %in% c("06","07","08","09"))
fig <- ggplot(data.plot, aes(x= as.Date(format(observation_date, "%m-%d"), "%m-%d"), y = as.numeric(observation_count), color = common_name))
fig <- fig + geom_point() #+ geom_line()
#fig <- fig + geom_smooth(method = "loess", se=F)
#fig <- fig + facet_grid(format(observation_date, "%Y")~., scales="free")
fig <- fig + theme(axis.text.x = element_text(angle = 90, hjust = 1))
fig <- fig + scale_x_date(date_breaks = "1 week", date_labels = "%b %d")
fig <- fig + xlab("Date") + ylab("Number of phalarope")
fig

##find peak 

##SALT POND DATABASE PHALAROPES
##LOAD SALT POND DATA
if (exists(x="dat.complete")==F) {
  source('Code_load_waterbird_data_13Dec2018.R')
}
head(dat.complete)
data.sp<-dat.complete ##salt pond data
data.sp<-subset(data.sp, SpeciesCode %in% c("RNPH", "PHAL", "REPH", "WIPH") & footprint=="All") ##restrict to phal
data.sp<-data.sp %>% group_by(MonthYear, Season, YearID, CountDate, Pond, Agency, SpeciesCode, StandardGuild, year, season.yr, complex, footprint) %>% dplyr::summarise(abun=sum(TotalAbundance, na.rm=T)) %>% data.frame() ##sum counts by species, date, and pond

##plot salt pond counts
data.plot<-subset(data.sp, format(CountDate, "%m") %in% c("06","07","08","09") & SpeciesCode %in% c("RNPH", "WIPH"))

data.plot<-subset(data.sp, SpeciesCode %in% c("RNPH", "WIPH") & year >2014)
fig <- ggplot(data.plot, aes(x= as.Date(format(CountDate, "%m-%d"), "%m-%d"), y = as.numeric(abun), color = SpeciesCode))
fig <- fig + geom_point() #+ geom_line()
#fig <- fig + geom_smooth(method = "loess", se=F)
fig <- fig + facet_grid(year~., scales="free")
fig <- fig + theme(axis.text.x = element_text(angle = 90, hjust = 1))
fig <- fig + scale_x_date(date_breaks = "1 week", date_labels = "%b %d")
fig <- fig + xlab("Date") + ylab("Number of phalarope")
#fig <- fig + scale_color_manual(values=c("magenta", "turquoise"), name="Species Code")
fig

##MAP
plot(y = data$latitude, x = data$longitude)

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

box.expand<-cbind(summary(ponds.poly)$bbox[,1]*0.9995, summary(ponds.poly)$bbox[,2]*1.0005); colnames(box.expand)<-c("min", "max")

box<-as.SpatialPolygons.bbox(bbox = box.expand, proj4stringFrom = CRS(proj4string(ponds.poly)), proj4stringTo = CRS(proj4string(land.utm)))
if (exists("land.clip")==F) {
  land.clip<-gIntersection(spgeom1 = land.utm, spgeom2=box)
}

library(dplyr)

ponds.poly.df<-ponds.poly
ponds.poly.df@data$id <- rownames(ponds.poly.df@data)
ponds.f <- fortify(ponds.poly.df, region = "id")
ponds.df <- plyr::join(ponds.f, ponds.poly.df@data, by = "id")
ponds.df$Pond<-gsub(pattern = "N4Aa", replacement = "N4AA", x = ponds.df$Pond)
ponds.df$Pond<-gsub(pattern = "N4Ab", replacement = "N4AB", x = ponds.df$Pond)
ponds.df$Pond<-gsub(pattern = "R5S", replacement = "RS5", x = ponds.df$Pond)

##prep sighting locations
locs.ll<-subset(data, select=c("longitude", "latitude"))
locs.ll<-SpatialPoints(locs.ll)
proj4string(locs.ll) <- CRS("+init=epsg:4326")
locs.utm<-spTransform(locs.ll, CRSobj = proj4string(land.clip)) ##reproject

##map
plot(land.clip, add=F)
plot(locs.utm, add=T)

map <- ggplot(data=data.sp) + coord_equal()
map <- map + geom_polygon(aes(long, lat, group=group), data=land.clip, fill="light grey")
map <- map + geom_polygon(data= ponds.df, aes(long, lat, group=group), color="black", fill="grey")
map <- map + geom_point(data = data.frame(coordinates(locs.utm), Species = data$common_name), aes(longitude, latitude, color=Species))
map <- map + scale_x_continuous(name = "UTM E-W (m)", limits = c(summary(box)$bbox[1,1]-1, summary(box)$bbox[1,2]+1), expand = c(0,0))
map <- map + scale_y_continuous(name = "UTM N-S (m)", limits = c(summary(box)$bbox[2,1]-1, summary(box)$bbox[2,2]+1), expand = c(0,0))
map <- map + theme_classic() #+ theme(panel.background = element_rect(fill= "grey"))
map <- map + geom_path(aes(long, lat), data = box)
map <- map + annotate("text", label = "San Francisco\n Bay", x = 575700, y = 4154000, size = 3, colour = "dark blue")
#map <- map + theme(legend.position = c(0.85, 0.85))
#map <- map + facet_wrap(facets = ~frac)
map <- map + labs(fill='Pond Complex') 
#map <- map + theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8), axis.text.y = element_text(size=8))
map <- map + theme(axis.text.x=element_blank(), axis.text.y = element_blank())
map <- map + theme(legend.position = c(0.76, 0.9))
map

#png(filename = str_c(file.path, "/map.png"), units="in", width=6.5, height=7,  res=400);print(map); dev.off()
