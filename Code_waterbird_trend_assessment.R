##waterbird trend assessment
##prepared by M Tarjan
##July 24, 2018
##Prepared for SCVWD D3 minigrant and CA State Coastal Conservancy grant

##objectives: 
##1) Create an assessment of waterbird populations in relation to targets
##2) Assess targets for nesting birds using CWB data

##LOAD REQUIRED PACAKGES
library(RODBC) ##required to connect to Access database
library(dplyr)
library(ggplot2) ##required for plots
library(stringr)
library(tidyr) ##required for spread

##LOAD SALT POND DATA

wb<-"S:/Science/Waterbird/Databases - enter data here!/Cargill Pond Surveys/USGS data from Cheryl 29Jan2018/USGS_SFBBO_pond_data_26Feb2018.accdb" ##database filepath

con<-odbcConnectAccess2007(wb) ##open connection to database

sqlTables(con, tableType="TABLE")$TABLE_NAME ##Get names of available tables

qry<-
  "SELECT d.MonthYear, d.Season, d.YearID, d.CountDate, d.Pond, d.Agency, d.PondGrid, d.SpeciesCode, d.TotalAbundance, s.StandardGuild 
FROM SBSPBirdData_IncludesNoBirdPondCounts AS d
LEFT OUTER JOIN SpeciesCodes AS s ON d.SpeciesCode = s.SpeciesCode" 

dat<-sqlQuery(con, qry); head(dat) ##import the queried table

##when finished with db, close the connection
odbcCloseAll()

##format data
dat$year<-format(dat$MonthYear, "%Y")

##fix speciescode typos
dat$SpeciesCode<-as.character(dat$SpeciesCode)
translator <- c('DCCo' = 'DCCO', 
                'Phal' = 'PHAL',
                'wesa' = 'WESA',
                'saph' = 'SAPH',
                'phal' = 'PHAL') ##old = update
acronyms<-translator[dat$SpeciesCode]
dat$SpeciesCode[which(acronyms!='NA')]<-acronyms[which(acronyms!='NA')]

##add counts of zero for "none" category??
##stopped using "none" categroy in 2015 on. what was it replaced with? victoria will know how we enter these data into our database, but how did it get translated into USGS database when we transferred the data over?
##none category appears to apply at the GRID level

##use data from sites inside the SBSPRP footprint only
dat.sub<-subset(dat, subset = str_sub(Pond, 1,1) %in% c("A", "B", "R"), select=c(MonthYear, Season, YearID, year, CountDate, Pond, Agency, PondGrid, SpeciesCode, StandardGuild, TotalAbundance))

##need to consider years with counts at all ponds, or use a method that takes site into account
dat.complete<-dat.sub

##add season.year ids, taking into account that winter crosses years
dat.complete$season.yr<-str_c(dat.complete$Season, ".", as.character(dat.complete$year))
dat.complete$season.yr[which(format(dat.complete$MonthYear, "%m") %in% c("01", "02"))]<-str_c(dat.complete$Season[which(format(dat.complete$MonthYear, "%m") %in% c("01", "02"))], ".", as.character(as.numeric(dat.complete$year[which(format(dat.complete$MonthYear, "%m") %in% c("01", "02"))])-1))

##species counts by survey and pond
dat.pond<-dat.complete %>% group_by(year, season.yr, MonthYear, Season, Pond, SpeciesCode) %>% summarise(abun=sum(TotalAbundance)) %>% data.frame()

##species counts by unique survey
dat.spp<- dat.complete %>% group_by(year, season.yr, MonthYear, Season, SpeciesCode) %>% summarise(abun=sum(TotalAbundance)) %>% data.frame()

##average counts by season and species
dat.season<- dat.spp %>% group_by(Season, season.yr, SpeciesCode) %>% summarise(mean=round(mean(abun),0)) %>% data.frame()

##add ln abun to dat.spp and dat.season
dat.spp$ln.abun<-log(dat.spp$abun+1)
dat.season$ln.mean<-log(dat.season$mean+1)

##plot data for a given species
dat.spp.sub<-subset(dat.spp, SpeciesCode =="RUDU" & year >=2005)

##linear model of species over surveys
lm.spp<-lm(formula = ln.abun ~ MonthYear + Season, data = dat.spp.sub)

##ggplot of abundance and linear model
fig <- ggplot(data = dat.spp.sub, aes(x= MonthYear, y=ln.abun))
fig <- fig + geom_point()
fig <- fig + geom_abline(intercept = lm.spp$coefficients[1], slope=lm.spp$coefficients[2])
fig <- fig + geom_line()
fig

summary(lm.spp)

##model of counts
M0<-lm(formula = log(abun+1) ~ MonthYear + Season + Pond, data = subset(dat.pond, SpeciesCode=="RUDU"))
summary(M0)
prediction<-exp(predict(object = M0, newdata = data.frame(MonthYear = max(dat.pond$MonthYear), Season = "Winter", Pond = "AB2")))-1 ##prediction is for a certain pond. needs to be made for all ponds and then summed for the survey

fig <- ggplot(data = dat.spp.sub, aes(x= MonthYear, y=abun))
fig <- fig + geom_point()
#fig <- fig + geom_abline(intercept = lm.spp$coefficients[1], slope=lm.spp$coefficients[2])
fig <- fig + geom_line()
fig <- fig + geom_point(data = data.frame(MonthYear=max(dat.pond$MonthYear), abun=prediction), color="red", pch=4, size=5)
fig

##LOAD COLONIAL WATERBIRD DATA
