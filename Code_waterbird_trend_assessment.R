##waterbird trend assessment
##prepared by M Tarjan
##July 24, 2018
##Prepared for SCVWD D3 minigrant and CA State Coastal Conservancy grant

##objectives: 
##1) Create an assessment of waterbird populations in relation to targets
##2) Assess targets for nesting birds using CWB data

##LOAD REQUIRED PACAKGES
library(dplyr)
library(ggplot2) ##required for plots
library(tidyr) ##required for spread

##LOAD SALT POND DATA
source('Code_load_waterbird_data_13Dec2018.R')
head(dat.complete)

##use data from sites inside the SBSPRP footprint only
dat.sub<-subset(dat.complete, subset = str_sub(Pond, 1,1) %in% c("A", "B", "R"), select=c(MonthYear, Season, YearID, year, CountDate, Pond, Agency, PondGrid, SpeciesCode, StandardGuild, TotalAbundance))

dat<-dat.sub

##species counts by survey and pond
dat.pond<-dat %>% group_by(year, season.yr, MonthYear, Season, Pond, SpeciesCode) %>% summarise(abun=sum(TotalAbundance)) %>% data.frame()

##species counts by unique survey
dat.spp<- dat %>% group_by(year, season.yr, MonthYear, Season, SpeciesCode) %>% summarise(abun=sum(TotalAbundance)) %>% data.frame()

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
