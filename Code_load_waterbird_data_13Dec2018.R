##load salt pond data
##m tarjan
##Feb 4, 2019

##LOAD REQUIRED PACAKGES
library(RODBC) ##required to connect to Access database
library(stringr)

##LOAD SALT POND DATA
wb<-"S:/Science/Waterbird/Databases - enter data here!/Cargill Pond Surveys/USGS data from Cheryl 29Jan2018/USGS_SFBBO_pond_data_26Feb2018.accdb" ##database filepath

con<-odbcConnectAccess2007(wb) ##open connection to database

sqlTables(con, tableType="TABLE")$TABLE_NAME ##Get names of available tables

qry<-
  "SELECT d.MonthYear, d.Season, d.YearID, d.CountDate, d.Pond, d.Agency, d.PondGrid, d.SpeciesCode, d.TotalAbundance, s.StandardGuild, d.CountStartTime, d.CountEndTime 
FROM SBSPBirdData_IncludesNoBirdPondCounts AS d
LEFT OUTER JOIN SpeciesCodes AS s ON d.SpeciesCode = s.SpeciesCode" 

dat<-sqlQuery(con, qry); head(dat) ##import the queried table

##import zero bird count data
qry<-
  "SELECT d.MonthYear, d.Season, d.YearID, d.CountDate, d.Pond, d.Agency, d.PondGrid, d.SpeciesCode, d.TotalAbundance, s.StandardGuild, d.CountStartTime, d.CountEndTime 
FROM SBSPBirdData_ListOfNoBirdPondCounts AS d
LEFT OUTER JOIN SpeciesCodes AS s ON d.SpeciesCode = s.SpeciesCode" 

dat2<-sqlQuery(con, qry); head(dat2) ##import the queried table

##when finished with db, close the connection
odbcCloseAll()

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
##2015 on "none" category doesn't appear in data includingnobirdcounts, only in the table for no bird count info
##none category appears to apply at the GRID level through 2014
##set all pond grids for species ==none to "NOGRID"
dat2$PondGrid<-as.character(dat2$PondGrid)
dat2$PondGrid[which(dat2$SpeciesCode=="NONE")]<-"NOGRID"
##select unique dat.complete entries
dat2<-unique(dat2)

spp.guild<-unique(subset(dat, SpeciesCode != "NONE", select=c(SpeciesCode, StandardGuild))) ##get unique species and guild combos

out<-dim(0)
for (j in 1:nrow(dat2)) { ##for each pond/survey where no birds were observed
  dat.temp<-dat2[j,] %>% dplyr::slice(rep(1:n(), each = nrow(spp.guild)))
  ##add an observation of 0 for each species
  dat.temp$SpeciesCode<-spp.guild$SpeciesCode
  dat.temp$StandardGuild<-spp.guild$StandardGuild
  out<-rbind(out, dat.temp)
}

##add observations of zero to all data, after removing existing and incomplete zeros from data
dat<-rbind(subset(dat, SpeciesCode!="NONE"), out)

##format data
dat$year<-format(dat$MonthYear, "%Y")

dat.complete<-dat

##add season.year ids, taking into account that winter crosses years
dat.complete$season.yr<-str_c(dat.complete$Season, ".", as.character(dat.complete$year))
dat.complete$season.yr[which(format(dat.complete$MonthYear, "%m") %in% c("01", "02"))]<-str_c(dat.complete$Season[which(format(dat.complete$MonthYear, "%m") %in% c("01", "02"))], ".", as.character(as.numeric(dat.complete$year[which(format(dat.complete$MonthYear, "%m") %in% c("01", "02"))])-1))

##add duration of survey
dat.complete$duration.mins<-as.numeric(difftime(time2 = dat.complete$CountStartTime, time1 = dat.complete$CountEndTime, units="mins"))
dat.complete$duration.mins[which(strftime(dat.complete$CountStartTime, format="%H:%M:%S")=="00:00:00" | strftime(dat.complete$CountEndTime, format="%H:%M:%S")=="00:00:00")]<-NA ##set unknown durations to NA


##LOAD SFBBO WATERBIRD DATA
wb.sfbbo<-"S:/Science/Waterbird/Databases - enter data here!/Cargill Pond Surveys/Cargill Pond Surveys.accdb" ##database filepath

con<-odbcConnectAccess2007(wb.sfbbo) ##open connection to database

sqlTables(con, tableType="TABLE")$TABLE_NAME ##Get names of available tables

qry<-
  "SELECT * 
FROM [Bird Data] AS d" 

dat<-sqlQuery(con, qry); head(dat) ##import the queried table

##when finished with db, close the connection
odbcCloseAll()

dat.sfbbo<-dat

##format data
dat.sfbbo$year<-format(dat.sfbbo$Date, "%Y")
dat.sfbbo$month<-format(dat.sfbbo$Date, "%m")

##add duration of survey
dat.sfbbo$duration.mins<-as.numeric(difftime(time2 = dat.sfbbo$`Start Time`, time1 = dat.sfbbo$`End Time`, units="mins"))
dat.sfbbo$duration.mins[which(strftime(dat.sfbbo$`Start Time`, format="%H:%M:%S")=="00:00:00" | strftime(dat.sfbbo$`End Time`, format="%H:%M:%S")=="00:00:00")]<-NA ##set unknown durations to NA
dat.sfbbo$total.birds<-dat.sfbbo$`#Foraging` + dat.sfbbo$`#Roosting` + dat.sfbbo$`#On an Island` + dat.sfbbo$`#On a Levee` + dat.sfbbo$`#On Manmade`
