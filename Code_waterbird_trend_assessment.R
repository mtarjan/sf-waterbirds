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
dat<-dat.complete
dat$complex<-str_sub(dat$Pond, 1, 1)
dat$footprint<-"SBSPRP"
dat$footprint[which(dat$complex %in% c("M", "N"))]<-"Salt ponds"
dat2<-subset(dat, year >=2005); dat2$footprint<-"All"
dat<-rbind(dat, dat2)

##sum by species/guild, survey, and complex
dat.guild<-dat %>% group_by(MonthYear, Season, year, season.yr, footprint, StandardGuild) %>% dplyr::summarise(abun=sum(TotalAbundance, na.rm=T)) %>% data.frame()
dat.spp<-dat %>% group_by(MonthYear, Season, year, season.yr, footprint, SpeciesCode) %>% dplyr::summarise(abun=sum(TotalAbundance, na.rm=T)) %>% data.frame()

dat.guild$Species.Guild<-dat.guild$StandardGuild
dat.spp$Species.Guild<-dat.spp$SpeciesCode
#dat.spp.guild<-rbind(subset(dat.guild, select=-StandardGuild), subset(dat.spp, select=-SpeciesCode))


targets<-read.csv("targets.csv")

count.trends<-dim(0)
for (j in 1:nrow(targets)) {
  if (targets$SpeciesCode[j]=="") {
    dat.temp<- subset(dat.guild, Species.Guild == as.character(targets$Species.Guild)[j] & Season == targets$Season[j], select = -StandardGuild)
  } else {
    dat.temp<- subset(dat.spp, Species.Guild == as.character(targets$Species.Guild)[j] & Season == targets$Season[j], select= -SpeciesCode)
  }
  
  ##attempt to add zero counts
  dat.temp.zero<-subset(dat.spp, Species.Guild == "NONE" & Season == targets$Season[j], select= -SpeciesCode) ##select all records of no birds in that season
  dat.temp<-rbind(dat.temp, dat.temp.zero)
  dat.temp<-dat.temp[which(duplicated(subset(dat.temp, select = c(MonthYear, footprint)))==F),]
  dat.temp$Species.Guild<-targets$Species.Guild[j]

  
  ##calc percent change and baseline
  targets$Baseline[j] <- round(mean(subset(dat.temp, footprint =="All" & str_sub(season.yr, -4, -1) %in% c("2005", "2006", "2007"))$abun),0)
  #final.temp<-predict(object = lm(abun~MonthYear, data= subset(dat.temp, footprint=="All")), newdata = data.frame(MonthYear = max(dat.temp$MonthYear)))
  final.temp<-round(mean(subset(dat.temp, footprint =="All" & str_sub(season.yr, -4, -1) == as.character(max(as.numeric(year))))$abun),0)
  targets$Change[j]<-round((final.temp-targets$Baseline[j])/targets$Baseline[j]*100,0)
  
  ##determine whether counts in the last three years present a trigger
  trig.counts<-subset(dat.temp, footprint =="All" & str_sub(season.yr, -4, -1) %in% as.character(max(as.numeric(dat.temp$year)):(max(as.numeric(dat.temp$year))-2))) %>% group_by(season.yr) %>% dplyr::summarise(av=mean(abun)) %>% data.frame()
  if (nrow(subset(trig.counts, av < targets$Baseline[j]))>2) {targets$Trigger[j]<-T} else{targets$Trigger[j]<-F}
  
  ##get data for plotting
  count.trends<-rbind(count.trends, dat.temp)
}

guilds.plot<-unique(targets$Species.Guild)

##see all guilds
fig <- ggplot(data = subset(count.trends, Species.Guild %in% guilds.plot), aes(x = MonthYear, y = abun, color=footprint))
fig <- fig + geom_point()
fig <- fig + geom_smooth(method = "loess", se = F)
fig <- fig + facet_wrap(Species.Guild~Season, scales= "free")
fig <- fig + theme_classic()
fig <- fig + scale_color_manual(values=c("black", "forestgreen", "blue"), name="Pond area")
fig <- fig + geom_hline(data = subset(targets, Species.Guild %in% guilds.plot), linetype = "dashed", aes(yintercept=Baseline))
fig <- fig + theme(axis.text.x = element_text(angle = 45, hjust=1, color="black"), axis.text.y = element_text(color="black"))
fig <- fig + scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")
fig <- fig + scale_y_continuous(breaks = function(x) round(seq(from = 0,to = x[2]*1.2,by = (x[2]-0)/10),0), expand = c(0, 0))
fig <- fig + theme(strip.background = element_rect(colour = "white", fill = "white"))
fig <- fig + xlab("Year") + ylab("Number of Birds")
fig <- fig + theme(legend.position = "bottom")
fig

##write out plots for groups of guilds
guilds.plot<-list(ducks=c("DABBLER", "DIVER", "RUDU"), shorebirds=c("MEDSHORE", "SMSHORE"), other=c("LETE", "BOGU", "PHAL", "EAGR"))

for (j in 1:length(guilds.plot)) {
  fig <- ggplot(data = subset(count.trends, Species.Guild %in% guilds.plot[[j]]), aes(x = MonthYear, y = abun, color=footprint))
  fig <- fig + geom_point()
  fig <- fig + geom_smooth(method = "loess", se = F)
  fig <- fig + facet_wrap(Species.Guild~Season, scales= "free")
  fig <- fig + theme_classic()
  fig <- fig + scale_color_manual(values=c("black", "forestgreen", "blue"), name="Pond area")
  fig <- fig + geom_hline(data = subset(targets, Species.Guild %in% guilds.plot[[j]]), linetype = "dashed", aes(yintercept=Baseline))
  fig <- fig + theme(axis.text.x = element_text(angle = 45, hjust=1, color="black"), axis.text.y = element_text(color="black"))
  fig <- fig + scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")
  fig <- fig + scale_y_continuous(breaks = function(x) round(seq(from = 0,to = x[2]*1.2,by = (x[2]-0)/10),0), expand = c(0, 0))
  fig <- fig + theme(strip.background = element_rect(colour = "white", fill = "white"))
  fig <- fig + xlab("Year") + ylab("Number of Birds")
  fig <- fig + theme(legend.position = "bottom")
  fig
  
  fig.height<-c(3, 3, 6.5)
  
  png(filename = str_c(file.path, "/fig.loess.", names(guilds.plot)[j],".png"), units="in", width=6.5, height=fig.height[j],  res=200);print(fig); dev.off()
}

##model of counts
#M0<-lm(formula = log(abun+1) ~ MonthYear + Season + Pond, data = subset(dat.pond, SpeciesCode=="RUDU"))
#summary(M0)
#prediction<-exp(predict(object = M0, newdata = data.frame(MonthYear = max(dat.pond$MonthYear), Season = "Winter", Pond = "AB2")))-1 ##prediction is for a certain pond. needs to be made for all ponds and then summed for the survey

##LOAD COLONIAL WATERBIRD DATA
