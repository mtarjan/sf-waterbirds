##phalarope survey plan
##M Tarjan
##June 18, 2019

##a package that processes ebird data: http://strimas.com/ebird-best-practices/ebird.html#ebird-extract

library(ggplot2)

#install.packages("auk")
library(auk)
auk::auk_set_ebd_path("S:/Science/Waterbird/Program Folders (Gulls, SNPL, ADPP, etc)/Cargill Pond Surveys/Reports/2019 Waterbird Trend Assessment/sf-waterbirds/phalarope_ebird_data/", overwrite = T)

ebd<-auk_ebd(file = "ebd_US-CA-001_renpha_201501_201906_relMay-2019.txt")
##define filters
ebd_filters <- ebd %>% 
  #auk_species("Wood Thrush") %>% 
  ## southeastern coastal plain bcr
  #auk_bcr(bcr = 27) %>% 
  ## june, use * to get data from any year
  #auk_date(date = c("*-06-01", "*-06-30")) %>% 
  ## restrict to the standard traveling and stationary count protocols
  ##auk_protocol(protocol = c("Stationary", "Traveling")) %>% 
  auk_complete()
ebd_filters

##skip pervious code and just read in pre-subsetted file
data<-read_ebd("S:/Science/Waterbird/Program Folders (Gulls, SNPL, ADPP, etc)/Cargill Pond Surveys/Reports/2019 Waterbird Trend Assessment/sf-waterbirds/phalarope_ebird_data/ebd_US-CA-001_renpha_201501_201906_relMay-2019.txt")
data2<-read_ebd("S:/Science/Waterbird/Program Folders (Gulls, SNPL, ADPP, etc)/Cargill Pond Surveys/Reports/2019 Waterbird Trend Assessment/sf-waterbirds/phalarope_ebird_data/location2/ebd_US-CA-085_renpha_201501_201906_relMay-2019.txt")

data<-rbind(data.frame(data), data.frame(data2))

##plot number of birds observed over time
fig <- ggplot(data, aes(x= format(observation_date, "%m-%d"), y = as.numeric(observation_count), color = county))
fig <- fig + geom_point() + geom_line()
fig <- fig + facet_grid(format(observation_date, "%Y")~., scales="free")
fig <- fig + theme(axis.text.x = element_text(angle = 90, hjust = 1))
fig

##find peak 