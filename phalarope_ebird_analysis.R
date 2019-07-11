##phalarope survey plan
##M Tarjan
##June 18, 2019

##a package that processes ebird data: http://strimas.com/ebird-best-practices/ebird.html#ebird-extract

library(ggplot2)
library(stringr)
library(dplyr)

#install.packages("auk")
library(auk)
#auk::auk_set_ebd_path("S:/Science/Waterbird/Program Folders (Gulls, SNPL, ADPP, etc)/Cargill Pond Surveys/Reports/2019 Waterbird Trend Assessment/sf-waterbirds/phalarope_ebird_data")

##EBIRD DATA LOADING
data.folders<-"S:/Science/Waterbird/Program Folders (Gulls, SNPL, ADPP, etc)/Cargill Pond Surveys/Reports/2019 Waterbird Trend Assessment/sf-waterbirds/phalarope_ebird_data/"
data<-dim(0)
for (j in 1:length(list.files(data.folders))) {
  data.folder<-str_c(data.folders, list.files(data.folders)[j], "/")
  file.temp<-list.files(data.folder)[which(str_detect(string = list.files(data.folder), pattern = "ebd"))]
  data.temp<-read_ebd(str_c(data.folder, file.temp))
  data<-rbind(data, data.frame(data.temp))
}

##SALT POND DATABASE PHALAROPES
##LOAD SALT POND DATA
if (exists(x="dat.complete")==F) {
  source('Code_load_waterbird_data_13Dec2018.R')
}
head(dat.complete)
data.sp<-dat.complete ##salt pond data
data.sp<-subset(data.sp, SpeciesCode %in% c("RNPH", "PHAL", "REPH", "WIPH") & footprint=="All") ##restrict to phal
data.sp<-data.sp %>% group_by(MonthYear, Season, YearID, CountDate, Pond, Agency, SpeciesCode, StandardGuild, year, season.yr, complex, footprint) %>% dplyr::summarise(abun=sum(TotalAbundance, na.rm=T)) %>% data.frame() ##sum counts by species, date, and pond

##add locations
pond.coords<-read.csv("pond.coordinates.csv") ##coordinates of each pond grid
data.sp.ll<-dplyr::left_join(x = data.sp, y = pond.coords, by = c("Pond"= "pond")) ##join the coordinates to the bird data. need to specific the names of the columns with the grid names in both dataframes in the "by" argument

##add species name
translator <- c('PHAL' = 'Phalaropus spp', 
                'RNPH' = 'Phalaropus lobatus',
                'REPH' = 'Phalaropus fulicarius',
                'WIPH' = 'Phalaropus tricolor') ##old = update
acronyms<-translator[data.sp.ll$SpeciesCode]
data.sp.ll$scientific_name[which(acronyms!='NA')]<-acronyms[which(acronyms!='NA')]

##COMBINE DATASETS
phal<-subset(data.sp.ll, select=c("scientific_name", "abun", "y", "x", "CountDate")) %>% rename(observation_count=abun, longitude=x, latitude=y, observation_date=CountDate)
phal$source<-"SFBBO"
data$source<-"eBird"
phal<-rbind(phal, subset(data, select=c("scientific_name", "observation_count", "latitude", "longitude", "observation_date", "source")))

#select data in south bay only
phal.sb<-subset(phal, latitude >= 37.39884 & latitude <= 37.64138 & longitude >= -122.23960 & longitude <= -121.94052 & scientific_name %in% c("Phalaropus lobatus", "Phalaropus tricolor"))

##PLOTS
##plot number of birds observed over time
data.plot<-subset(phal.sb, format(observation_date, "%m") %in% c("06","07","08","09") & scientific_name %in% c("Phalaropus lobatus", "Phalaropus tricolor"))
#data.plot<-data
fig <- ggplot(data.plot, aes(x= as.Date(format(observation_date, "%m-%d"), "%m-%d"), y = as.numeric(observation_count), color = scientific_name))
fig <- fig + geom_point() #+ geom_line()
#fig <- fig + geom_smooth(method = "loess", se=F)
#fig <- fig + facet_grid(format(observation_date, "%Y")~., scales="free")
fig <- fig + facet_grid(source~., scales="free")
fig <- fig + theme(axis.text.x = element_text(angle = 90, hjust = 1))
fig <- fig + scale_x_date(date_breaks = "1 week", date_labels = "%b %d")
fig <- fig + xlab("Date") + ylab("Number of phalarope")
fig

##find peak date by species
data.pred<-dim(0)
survey.date<-dim(0)
data.curve<-dim(0)
for (j in 1:length(unique(phal.sb$scientific_name))) { ##for each species
  sp.temp<-unique(phal.sb$scientific_name)[j]
  data.temp<-subset(phal, scientific_name==sp.temp & source=="eBird")
  data.temp$doy<-as.numeric(strftime(data.temp$observation_date, format = "%j")) ##create of day of year variable
  ##subset to select dates of interest (late summer/early fall)
  data.temp<-subset(data.temp, doy > 160 & doy < 300)
  ##add numeric count and remove NA observations
  data.temp$count<-as.numeric(data.temp$observation_count)
  data.temp<-subset(data.temp, is.na(count)==F)
  ##fit model
  model.temp<-nls(count ~ k*exp(-1/2*(doy-mu)^2/sigma^2), start=c(mu=200,sigma=25,k=220), data = data.temp, control = list(maxiter = 100))
  data.temp$pred<-predict(model.temp, newdata=data.temp)
  data.pred<-rbind(data.pred, data.temp)
  pred.temp<-data.frame(scientific_name=sp.temp, doy=min(data.temp$doy):max(data.temp$doy))
  pred.temp$pred<-predict(model.temp, newdata=pred.temp)
  data.curve<-rbind(data.curve, pred.temp)
  
  #plot(data.temp$doy, data.temp$count); points(data.temp$doy, data.temp$pred, col="red")
  ##estimate date of peak count
  max.temp<-data.temp$observation_date[which.max(data.temp$pred)]
  survey.date<-rbind(survey.date, data.frame(species=unique(phal.sb$scientific_name)[j], max.date=format(max.temp, "%m-%d"), mu = coef(model.temp)[1], sigma=coef(model.temp)[2]))
}

##see date with max counts from model
survey.date

##plot counts with fitted curves
fig <- ggplot(data.pred, aes(x= as.Date(format(observation_date, "%m-%d"), "%m-%d"), y = as.numeric(observation_count), color = scientific_name))
fig <- fig + geom_point() 
#fig <- fig + geom_line(aes(x=as.Date(format(observation_date, "%m-%d"), "%m-%d"), y=pred*10))
fig <- fig + geom_line(data= data.curve, aes(x=as.Date(format(as.Date(as.character(doy), "%j"), "%m-%d"), "%m-%d"), y=pred*10))
#fig <- fig + stat_function(function(doy) k*exp(-1/2*(doy-mu)^2/sigma^2))
fig <- fig + theme(axis.text.x = element_text(angle = 90, hjust = 1))
fig <- fig + scale_x_date(date_breaks = "1 week", date_labels = "%b %d")
fig <- fig + xlab("Date") + ylab("Number of phalarope")
fig <- fig + scale_y_continuous(expand = c(0,0), sec.axis = sec_axis(~ . /10, name = "Fitted curve"))
fig

##plot salt pond counts
#data.plot<-subset(data.sp, format(CountDate, "%m") %in% c("06","07","08","09") & SpeciesCode %in% c("RNPH", "WIPH"))

#data.plot<-subset(data.sp, SpeciesCode %in% c("RNPH", "WIPH") & year >2014)
#fig <- ggplot(data.plot, aes(x= as.Date(format(CountDate, "%m-%d"), "%m-%d"), y = as.numeric(abun), color = SpeciesCode))
#fig <- fig + geom_point() #+ geom_line()
#fig <- fig + geom_smooth(method = "loess", se=F)
#fig <- fig + facet_grid(year~., scales="free")
#fig <- fig + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#fig <- fig + scale_x_date(date_breaks = "1 week", date_labels = "%b %d")
#fig <- fig + xlab("Date") + ylab("Number of phalarope")
#fig <- fig + scale_color_manual(values=c("magenta", "turquoise"), name="Species Code")
#fig

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

#box.expand<-cbind(summary(ponds.poly)$bbox[,1]*0.9995, summary(ponds.poly)$bbox[,2]*1.0005); colnames(box.expand)<-c("min", "max")
box.expand<-summary(ponds.poly)$bbox*cbind(c(0.99, 0.9995), c(1.0009, 1.0005))

box<-as.SpatialPolygons.bbox(bbox = box.expand, proj4stringFrom = CRS(proj4string(ponds.poly)), proj4stringTo = CRS(proj4string(land.utm)))
if (exists("land.clip")==F) {
  land.clip<-gIntersection(spgeom1 = land.utm, spgeom2=box)
}

ponds.poly.df<-ponds.poly
ponds.poly.df@data$id <- rownames(ponds.poly.df@data)
ponds.f <- fortify(ponds.poly.df, region = "id")
ponds.df <- plyr::join(ponds.f, ponds.poly.df@data, by = "id")
ponds.df$Pond<-gsub(pattern = "N4Aa", replacement = "N4AA", x = ponds.df$Pond)
ponds.df$Pond<-gsub(pattern = "N4Ab", replacement = "N4AB", x = ponds.df$Pond)
ponds.df$Pond<-gsub(pattern = "R5S", replacement = "RS5", x = ponds.df$Pond)

##prep sighting locations
locs.ll<-subset(phal.sb, select=c("longitude", "latitude"))
locs.ll<-SpatialPoints(locs.ll)
proj4string(locs.ll) <- CRS("+init=epsg:4326")
locs.utm<-spTransform(locs.ll, CRSobj = proj4string(land.clip)) ##reproject

##map
map <- ggplot(data=subset(phal.sb, as.numeric(format(observation_date, "%y")) > 14)) + coord_equal()
map <- map + geom_polygon(aes(long, lat, group=group), data=land.clip, fill="light grey")
map <- map + geom_polygon(data= ponds.df, aes(long, lat, group=group), color="black", fill="grey")
map <- map + geom_point(data = data.frame(coordinates(locs.utm), Species = phal.sb$scientific_name, Source=factor(phal.sb$source), Abundance=as.numeric(phal.sb$observation_count)), aes(longitude, latitude, color=Species, shape=Source, size=Abundance))
map <- map + scale_size_area(max_size=10)
map <- map + scale_x_continuous(name = "UTM E-W (m)", limits = c(summary(box)$bbox[1,1]-1, summary(box)$bbox[1,2]+1), expand = c(0,0))
map <- map + scale_y_continuous(name = "UTM N-S (m)", limits = c(summary(box)$bbox[2,1]-1, summary(box)$bbox[2,2]+1), expand = c(0,0))
map <- map + theme_classic() #+ theme(panel.background = element_rect(fill= "grey"))
map <- map + geom_path(aes(long, lat), data = box)
map <- map + annotate("text", label = "San Francisco\n Bay", x = 571500, y = 4160000, size = 5, colour = "dark blue")
#map <- map + theme(legend.position = c(0.85, 0.85))
#map <- map + facet_wrap(facets = ~frac)
map <- map + labs(fill='Pond Complex') 
#map <- map + theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8), axis.text.y = element_text(size=8))
map <- map + theme(axis.text.x=element_blank(), axis.text.y = element_blank())
map <- map + theme(legend.position = c(0.8, 0.7)) + guides(size = guide_legend(nrow = 2, byrow = T))
map

png(filename = str_c(file.path, "/map.png"), units="in", width=6.5, height=7,  res=400);print(map); dev.off()
