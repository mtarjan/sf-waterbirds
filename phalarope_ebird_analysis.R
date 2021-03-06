##phalarope survey plan
##M Tarjan
##June 18, 2019

##a package that processes ebird data: http://strimas.com/ebird-best-practices/ebird.html#ebird-extract

library(ggplot2)
library(stringr)
library(dplyr)
library(gam)

#install.packages("auk")
library(auk)
#auk::auk_set_ebd_path("S:/Science/Waterbird/Program Folders (Gulls, SNPL, ADPP, etc)/Cargill Pond Surveys/Reports/2019 Waterbird Trend Assessment/sf-waterbirds/eBird_data", overwrite=T)

##EBIRD DATA LOADING
#data.folders<-"S:/Science/Waterbird/Program Folders (Gulls, SNPL, ADPP, etc)/Cargill Pond Surveys/Reports/2019 Waterbird Trend Assessment/sf-waterbirds/phalarope_ebird_data/"
#data<-dim(0)
#for (j in 1:length(list.files(data.folders))) {
#  data.folder<-str_c(data.folders, list.files(data.folders)[j], "/")
#  file.temp<-list.files(data.folder)[which(str_detect(string = list.files(data.folder), pattern = "ebd"))]
#  data.temp<-read_ebd(str_c(data.folder, file.temp))
#  data<-rbind(data, data.frame(data.temp))
#}

##LOAD FROM FULL EBIRD DATASET
ebd <- auk_ebd("ebd_relMay-2019/ebd_relMay-2019/ebd_relMay-2019.txt", 
               file_sampling = "ebd_sampling_relMay-2019/ebd_sampling_relMay-2019/ebd_sampling_relMay-2019.txt")
ebd_filters <- ebd %>% 
  auk_species(c("Phalaropus lobatus", "Phalaropus fulicarius", "Phalaropus tricolor", "Phalaropus sp.")) %>% 
  # southeastern coastal plain bcr
  auk_state(state = "US-CA") %>% 
  # restrict to the standard traveling and stationary count protocols
  #auk_protocol(protocol = c("Stationary", "Traveling")) %>% 
  auk_complete()
ebd_filters

# output files
data_dir <- "data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}
f_ebd <- file.path(data_dir, "ebd_phal_CA.txt")
f_sampling <- file.path(data_dir, "ebd_phal_CA_sampling.txt")

# only run if the files don't already exist
if (!file.exists(f_ebd)) {
  auk_filter(ebd_filters, file = f_ebd, file_sampling = f_sampling)
}

ebd_zf <- auk_zerofill(f_ebd, f_sampling, collapse = TRUE)

data1<-data.frame(ebd_zf)
##restrict data to south bay for processing
data<-subset(data1, latitude >= 37.39884 & latitude <= 37.64138 & longitude >= -122.23960 & longitude <= -121.94052)

##alternatively, read in csv subsetted to south bay
#data<-read.csv("S:/Science/Waterbird/Program Folders (Gulls, SNPL, ADPP, etc)/Cargill Pond Surveys/Reports/2019 Waterbird Trend Assessment/sf-waterbirds/data/ebird_phal_sb_zf.csv")

##clean data
# function to convert time observation to hours since midnight
time_to_decimal <- function(x) {
  x <- hms(x, quiet = TRUE)
  hour(x) + minute(x) / 60 + second(x) / 3600
}

library(lubridate) ##required for time manipulation

# clean up variables
data <- data %>% 
  mutate(
    # convert X to NA
    observation_count = if_else(observation_count == "X", 
                                NA_character_, observation_count),
    observation_count = as.integer(observation_count),
    # effort_distance_km to 0 for non-travelling counts
    effort_distance_km = if_else(protocol_type != "Traveling", 
                                 0, effort_distance_km),
    # convert time to decimal hours since midnight
    time_observations_started = time_to_decimal(time_observations_started)
  )

# additional filtering
data <- data %>% 
  filter(
    # effort filters
    duration_minutes <= 5 * 60,
    effort_distance_km <= 5,
    # last 10 years of data
    #year(observation_date) >= 2009,
    # 10 or fewer observers
    number_observers <= 10)

##SALT POND DATABASE PHALAROPES
##LOAD SALT POND DATA
if (exists(x="dat.complete")==F) {
  source('Code_load_waterbird_data_13Dec2018.R')
}
head(dat.complete)
data.sp<-dat.complete ##salt pond data
##select only SFBBO data
data.sp<-subset(dat.complete, Agency!="USGS" & YearID>2013)
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
acronyms<-translator[as.character(data.sp.ll$SpeciesCode)]
data.sp.ll$scientific_name[which(acronyms!='NA')]<-acronyms[which(acronyms!='NA')]

##COMBINE DATASETS
phal<-subset(data.sp.ll, select=c("scientific_name", "abun", "y", "x", "CountDate")) %>% rename(observation_count=abun, longitude=x, latitude=y, observation_date=CountDate)
phal$source<-"SBSPRP"
data$source<-"eBird"
phal<-rbind(phal, subset(data, select=c("scientific_name", "observation_count", "latitude", "longitude", "observation_date", "source")))

#select data in south bay only
phal.sb<-subset(phal, latitude >= 37.39884 & latitude <= 37.64138 & longitude >= -122.23960 & longitude <= -121.94052 & as.numeric(format(observation_date, "%Y")) > 2005)

##number of observations of each species
table(subset(phal.sb, observation_count>0 & source=="SBSPRP")$scientific_name)

##select two species
phal.sb<-subset(phal.sb, scientific_name %in% c("Phalaropus lobatus", "Phalaropus tricolor"))

##PLOTS
##plot number of birds observed over time
#data.plot<-subset(phal.sb, format(observation_date, "%m") %in% c("06","07","08","09") & scientific_name %in% c("Phalaropus lobatus", "Phalaropus tricolor"))
data.plot<-subset(phal.sb, scientific_name %in% c("Phalaropus lobatus", "Phalaropus tricolor") & observation_count>0)
#data.plot<-data
fig <- ggplot(data.plot, aes(x= as.Date(format(observation_date, "%m-%d"), "%m-%d"), y = observation_count, color = scientific_name))
fig <- fig + geom_point() #+ geom_line()
#fig <- fig + geom_smooth(method = "loess", se=F)
fig <- fig + facet_grid(format(observation_date, "%Y")~source, scales="free")
#fig <- fig + facet_grid(source~., scales="free")
fig <- fig + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust =0.5))
fig <- fig + scale_x_date(date_breaks = "3 weeks", date_labels = "%b %d")
fig <- fig + xlab("Date") + ylab("Number of phalarope")
fig <- fig + labs(color="Species")
fig

png(filename = str_c(file.path, "/phal.counts.png"), units="in", width=6.5, height=8,  res=200);print(fig); dev.off()

##find peak date by species
data.pred<-dim(0)
survey.date.by.yr<-dim(0)
data.curve<-dim(0)
for (y in 2005:2018) { ##for each year
  for (j in 1:length(unique(phal.sb$scientific_name))) { ##for each species
    sp.temp<-unique(phal.sb$scientific_name)[j]
    data.temp<-subset(phal, scientific_name==sp.temp & source=="eBird" & year(phal$observation_date) == y)
    data.temp$doy<-as.numeric(strftime(data.temp$observation_date, format = "%j")) ##create of day of year variable
    ##subset to select dates of interest (late summer/early fall)
    data.temp<-subset(data.temp, doy > 160 & doy < 300)
    ##add numeric count and remove NA observations
    data.temp$count<-data.temp$observation_count
    data.temp<-subset(data.temp, is.na(count)==F)
    ##fit model
    model.temp<-gam::gam(count ~ s(doy), data = data.temp)
    data.temp$pred<-predict(model.temp, newdata=data.temp)
    data.pred<-rbind(data.pred, data.temp)
    pred.temp<-data.frame(scientific_name=sp.temp, doy=min(data.temp$doy):max(data.temp$doy))
    pred.temp$pred<-predict(model.temp, newdata=pred.temp)
    data.curve<-rbind(data.curve, pred.temp)
    
    ##estimate date of peak count
    max.temp<-data.temp$observation_date[which.max(data.temp$pred)]
    survey.date.by.yr<-rbind(survey.date.by.yr, data.frame(year=y, species=unique(phal.sb$scientific_name)[j], max.date=format(max.temp, "%m-%d"), n = nrow(data.temp), aic = summary(model.temp)$aic, df = summary(model.temp)$df[2]))
  }
}

##see date with max counts from model for each year
survey.date.by.yr

##create a model for all years pooled
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
  data.temp$count<-data.temp$observation_count
  data.temp<-subset(data.temp, is.na(count)==F)
  ##fit model
  model.temp<-gam::gam(count ~ s(doy), data = data.temp)
  data.temp$pred<-predict(model.temp, newdata=data.temp) ##get model predictions at each data point
  data.pred<-rbind(data.pred, data.temp)
  pred.temp<-data.frame(scientific_name=sp.temp, doy=min(data.temp$doy):max(data.temp$doy)) ##create distrib of points to get predictions thorughout the range of dates
  pred.temp$pred<-predict(model.temp, newdata=pred.temp) ##get model predictions across all days
  data.curve<-rbind(data.curve, pred.temp)
  
  ##estimate date of peak count
  max.temp<-data.temp$observation_date[which.max(data.temp$pred)]
  survey.date<-rbind(survey.date, data.frame(species=unique(phal.sb$scientific_name)[j], max.date=format(max.temp, "%m-%d"), n = nrow(data.temp), aic = summary(model.temp)$aic, df = summary(model.temp)$df[2]))
}

survey.date

##plot counts with fitted curves
fig <- ggplot(subset(data.pred, count<5000), aes(x= as.Date(format(observation_date, "%m-%d"), "%m-%d"), y = observation_count, color = scientific_name))
fig <- fig + geom_point() 
#fig <- fig + geom_line(aes(x=as.Date(format(observation_date, "%m-%d"), "%m-%d"), y=pred*10))
fig <- fig + geom_line(data= data.curve, aes(x=as.Date(format(as.Date(as.character(doy), "%j"), "%m-%d"), "%m-%d"), y=pred*100))
#fig <- fig + stat_function(function(doy) k*exp(-1/2*(doy-mu)^2/sigma^2))
fig <- fig + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
fig <- fig + scale_x_date(date_breaks = "1 week", date_labels = "%b %d")
fig <- fig + xlab("Date") + ylab("Number of phalarope")
fig <- fig + scale_y_continuous(expand = c(0,0), sec.axis = sec_axis(~ . /100, name = "Predicted count"))
fig <- fig + labs(color="Species")
fig <- fig + theme_classic()
fig <- fig + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust =0.5))
fig

png(filename = str_c(file.path, "/phal.curves.png"), units="in", width=6.5, height=4,  res=400);print(fig); dev.off()

#png(filename = str_c(file.path, "/phal.curves.poster.png"), units="in", width=6.5/1.25, height=4/1.25,  res=400);print(fig); dev.off()

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

##prep sighting locations
locs.ll<-subset(phal.sb, observation_count>0, select=c("longitude", "latitude"))
locs.ll<-SpatialPoints(locs.ll)
proj4string(locs.ll) <- CRS("+init=epsg:4326")
locs.utm<-spTransform(locs.ll, CRSobj = proj4string(land.clip)) ##reproject

data.plot<-subset(phal.sb, as.numeric(observation_count)>0)

##create KDE layer
library(ks)#; library(raster)
density<-kde(x = coordinates(locs.utm), w = data.plot$observation_count/sum(data.plot$observation_count)*nrow(data.plot))
grid<-dim(0)
for (j in 1:length(density$eval.points[[1]])) {
  grid<-rbind(grid, data.frame(x=rep(density$eval.points[[1]][j], length(density$eval.points[[2]])), y = density$eval.points[[2]], z = density$estimate[j,]))
}
density.r<-raster::rasterFromXYZ(grid)

##map
map <- ggplot(data=data.plot) + coord_equal()
map <- map + geom_polygon(aes(long, lat, group=group), data=land.clip, fill="light grey")
map <- map + geom_polygon(data= ponds.df, aes(long, lat, group=group), color="black", fill="grey")
#map <- map + geom_point(data = data.frame(coordinates(locs.utm), Species = phal.sb$scientific_name, Source=factor(phal.sb$source), Abundance=as.numeric(phal.sb$observation_count)), aes(longitude, latitude, color=Species, shape=Source, size=Abundance))
#map <- map + geom_raster(data=grid, aes(x=x, y=y, fill=z))
#map <- map + geom_tile(data=grid, aes(x=x, y=y, fill=z))
#map <- map + scale_fill_gradient(high = "red", low = "yellow")
map <- map + geom_point(data = data.frame(coordinates(locs.utm), Species = data.plot$scientific_name, Source=factor(data.plot$source), Abundance=as.numeric(data.plot$observation_count)), aes(longitude, latitude, size = Abundance), alpha=5/10, color="blue")
map <- map + facet_grid(facets = Source~Species)
map <- map + scale_size_area(max_size=8)
map <- map + scale_x_continuous(name = "UTM E-W (m)", limits = c(summary(box)$bbox[1,1]-1, summary(box)$bbox[1,2]+1), expand = c(0,0))
map <- map + scale_y_continuous(name = "UTM N-S (m)", limits = c(summary(box)$bbox[2,1]-1, summary(box)$bbox[2,2]+1), expand = c(0,0))
map <- map + theme_classic() #+ theme(panel.background = element_rect(fill= "grey"))
map <- map + geom_path(aes(long, lat), data = box)
map <- map + annotate("text", label = "San Francisco\n Bay", x = 571500, y = 4160000, size = 2.5, colour = "dark blue")
map <- map + theme(legend.position = "right")
map <- map + labs(fill='Pond Complex') 
map <- map + theme(axis.text.x=element_blank(), axis.text.y = element_blank())
#map <- map + theme(legend.position = c(0.8, 0.7)) + guides(size = guide_legend(nrow = 2, byrow = T))
map <- map + theme(axis.ticks=element_blank())
map

png(filename = str_c(file.path, "/map.png"), units="in", width=6.5, height=7,  res=400);print(map); dev.off()

##SITES TO SURVEY
data.pond<-data.sp.ll %>% group_by(Pond) %>% summarise(summed=sum(abun)) %>% data.frame()
data.pond<-data.pond[order(data.pond$summed, decreasing = T),]
for (j in 1:nrow(data.pond)) {
  per.temp<-sum(data.pond$summed[1:j])/sum(data.pond$summed)
  if (per.temp>0.99) {
    survey.sites<-data.pond$Pond[1:j]
    break
  }
}

##show number of times phalaropes were observated at each pond each year
table(subset(data.sp.ll, abun>0)$YearID, subset(data.sp.ll, abun>0)$Pond)

##map of sites to survey
sites.poly<-rgdal::readOGR(dsn = "S:/Science/Waterbird/Program Folders (Gulls, SNPL, ADPP, etc)/Cargill Pond Surveys/PHAL survey/PHAL data", layer= "Phalarope survey sites")
sites.poly <- spTransform(sites.poly, CRS(proj4string(ponds.poly))) # reproject
sites.poly<-sites.poly[sites.poly@data$Site.Name != "Crittenden Marsh East" & sites.poly@data$Site.Name != "Spreckles Marsh",]

##add site type to both sets of sites (SBSPRP and eBird)
ponds.poly@data$type<-"SBSPRP"
sites.poly@data$type<-"eBird"

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
map <- map + annotate("text", label = "San Francisco\n Bay", x = 571500, y = 4160000, size = 4, colour = "dark blue")
#map <- map + theme(axis.text.x=element_blank(), axis.text.y = element_blank())
#map <- map + theme(axis.ticks=element_blank())
map <- map + geom_text(aes(label = 1:nrow(all.sites.poly@data), x = V1, y = V2, fontface="bold"))
map <- map + annotation_custom(gridExtra::tableGrob(legend.text[1:(length(survey.sites)/3),], theme=mytheme), xmin=579000, xmax=590700, ymin=4150000, ymax=4167000)
map <- map + annotation_custom(gridExtra::tableGrob(legend.text[(length(survey.sites)/3+1):(length(survey.sites)/3*2),], theme=mytheme), xmin=584700, xmax=590700, ymin=4150000, ymax=4167000)
map <- map + annotation_custom(gridExtra::tableGrob(legend.text[(length(survey.sites)/3*2+1):(length(survey.sites)/3*3),], theme=mytheme), xmin=591100, xmax=591200, ymin=4150000, ymax=4167000)
map <- map + annotation_custom(gridExtra::tableGrob(legend.text[(length(survey.sites)+1):nrow(legend.text),], theme=mytheme), xmin=570000, xmax=576000, ymin=4140000, ymax=4149000)
map

png(filename = str_c(file.path, "/survey.sites.png"), units="in", width=6.5, height=7,  res=400);print(map); dev.off()

##SURVEY EFFORT
##area/ acreage of all sites and acreage of survey sites
site.area<-unique(subset(ponds.df, select=c(Pond, Area)))
sum(site.area$Area)/4047 ##convert to acres
sum(subset(site.area, Pond %in% survey.sites)$Area)/4047 ##convert to acres
sum(sites.poly@data$Area)/4047

##area of sites surveyed in 2020
phal.counts<-gdata::read.xls("S:/Science/Waterbird/Program Folders (Gulls, SNPL, ADPP, etc)/Cargill Pond Surveys/PHAL survey/PHAL data/Phalarope Data all years.xlsx", sheet="Phalarope Data")
phal.counts<-phal.counts[which(str_detect(phal.counts$Date, pattern = "2020")),]
sites2020<-unique(phal.counts$Site.name)
plot(all.sites.poly)
plot(subset(all.sites.poly, Site.Name %in% sites2020), add=T, col="red")
sum(subset(all.sites.poly, Site.Name %in% sites2020)$Area)/4047

##acreage of phalarope habitats in all of SF Bay
bay.habitats<-rgdal::readOGR(dsn = "S:/Science/GIS/EcoAtlas_v1.50b4__SFEI_1998_0/Modern Baylands", layer= "SFEI_modern_baylands_poly_with_crosswalk")
plot(subset(bay.habitats, LEVEL4 %in% c("Storage or Treatment Basin", "Muted Tidal Marsh", "Inactive Salt Pond", "Diked Marsh", "Active Salt Pond")))
##total area of suitable phal habitat in SF Bay
sum(subset(bay.habitats, LEVEL4 %in% c("Storage or Treatment Basin", "Muted Tidal Marsh", "Inactive Salt Pond", "Diked Marsh", "Active Salt Pond"))$ACREAGE)

##eBird TRENDS OVER TIME
data.plot<-subset(phal.sb, source=="eBird" & as.numeric(format(observation_date, "%Y")) < 2019 & format(observation_date, "%m") %in% c("06","07","08","09")) %>% group_by(scientific_name, year = as.numeric(format(observation_date, "%Y"))) %>% summarise(av = mean(as.numeric(observation_count), na.rm=T)) %>% data.frame() #mean ebird count
fig <- ggplot(data = data.plot, aes(x = year, y = av, color=scientific_name))
fig <- fig + geom_point() + geom_smooth(method="loess", se=F)
fig <- fig + scale_y_continuous(expand = c(0,0), limits = c(0,max(data.plot$av)*1.1))
fig <- fig + scale_x_continuous(breaks = seq(2004, 2019, 1))
fig <- fig + theme_classic()
fig <- fig + xlab("Year") + ylab("Mean number of phalaropes") + labs(color="Species")
fig <- fig + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust =0.5))
fig

png(filename = str_c(file.path, "/eBird.trend.png"), units="in", width=6.5, height=4,  res=400);print(fig); dev.off()

##restrict ebird observations to those within the ponds
##assign location info to ebird data
##create spatial points for ebird data
locs.temp<-subset(data, select=c("longitude", "latitude"))
locs.temp<-SpatialPoints(locs.temp)
proj4string(locs.temp) <- CRS("+init=epsg:4326")
locs.temp<-spTransform(locs.temp, CRSobj = proj4string(ponds.poly))
##check if they are within project polygons
ebird.pond.locs<-subset(over(x= locs.temp, y = ponds.poly), select=c("Pond", "Complex"))
##add pond info to ebird data
ebird.pond<-cbind(data, ebird.pond.locs)
ebird.pond<-subset(ebird.pond, is.na(Pond)==F & format(observation_date, "%Y")>2004) ##select ebird observations that are within the project footprint
rm(locs.temp)

##GAM TRENDS

##create a model for all years pooled
data.pred<-dim(0)
data.curve<-dim(0)

for (j in 2:length(unique(ebird.pond$scientific_name))) { ##for each species
  sp.temp<-unique(ebird.pond$scientific_name)[j]
  data.temp<-subset(ebird.pond, scientific_name==sp.temp)
  ##add numeric count and remove NA observations
  data.temp$count<-data.temp$observation_count
  data.temp<-subset(data.temp, is.na(count)==F)
  ##fit model
  model.temp<-gam::gam(count ~ s(observation_date), data = data.temp)
  data.temp$pred<-predict(model.temp, newdata=data.temp) ##get model predictions at each data point
  data.pred<-rbind(data.pred, data.temp)
  pred.temp<-data.frame(scientific_name=sp.temp, observation_date=min(data.temp$observation_date):max(data.temp$observation_date)) ##create distrib of points to get predictions thorughout the range of dates
  pred.temp$pred<-predict(model.temp, newdata=pred.temp) ##get model predictions across all days
  data.curve<-rbind(data.curve, pred.temp)
}

##plot counts with fitted curves
fig <- ggplot(ebird.pond, aes(x= observation_date, y = observation_count, color = scientific_name))
fig <- fig + geom_point() 
fig <- fig + geom_line(data= data.pred, aes(x=observation_date, y=pred*100))
fig <- fig + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
fig <- fig + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
fig <- fig + xlab("Date") + ylab("Number of phalarope")
fig <- fig + scale_y_continuous(expand = c(0,0), sec.axis = sec_axis(~ . /100, name = "Predicted count"))
fig <- fig + labs(color="Species")
fig <- fig + theme_classic()
fig <- fig + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust =0.5))
fig

png(filename = str_c(file.path, "/phal.ebird.trend.png"), units="in", width=6.5, height=4,  res=400);print(fig); dev.off()