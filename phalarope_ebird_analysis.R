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
plot(data$latitude, data$longitude)