##load salt pond data
##m tarjan
##Feb 4, 2019

##LOAD REQUIRED PACAKGES
library(RODBC) ##required to connect to Access database
library(stringr)
library(dplyr)
library(tidyr) ##required for spread

##CREATE FOLDER FOR FIGURES
dir.create(str_c("figures.", Sys.Date()))
file.path<-str_c("figures.", Sys.Date())

##LOAD SALT POND DATA
wb<-"S:/Science/Waterbird/Databases - enter data here!/Cargill Pond Surveys/USGS data from Cheryl 29Jan2018/USGS_SFBBO_pond_data_03Jul2019.accdb" ##database filepath

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

##create a count of zero for each species/guild based on "zero bird pond count"
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

dat.E<-dat.complete
##REPLACE B WITH E FOR EDEN
dat.E$Pond<-str_c(gsub(pattern = "B", replacement = "E", x = str_sub(dat.E$Pond, 1, 1)), str_sub(dat.E$Pond, 2))
dat.complete<-dat.E

##add footprint
dat.complete$complex<-str_sub(dat.complete$Pond, 1, 1)
dat.complete$footprint<-"SBSPRP"
dat.complete$footprint[which(dat.complete$complex %in% c("M", "N"))]<-"Salt ponds"
dat2<-subset(dat.complete, MonthYear >= min(subset(dat.complete, footprint=="Salt ponds")$MonthYear)); dat2$footprint<-"All"
dat.complete<-rbind(dat.complete, dat2)


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


##LOAD COLONIAL WATERBIRD DATA
wb<-"S:/Science/Waterbird/Databases - enter data here!/Colonial Waterbird Surveys/ColonialWaterbird.accdb"
con<-odbcConnectAccess2007(wb) ##open connection to database

qry<-
  "SELECT MetadataColonyLocation.ColonyName, DataSurveyInformation.SurveyDate, year(DataSurveyInformation.SurveyDate) AS year, MetadataSpeciesCode.SpeciesCode, DataSurveySummary.TotalNestCount, DataSurveySummary.TotalAdultCount, DataSurveySummary.TotalYoungCount 
FROM ((DataSurveySummary 
LEFT OUTER JOIN MetadataSpeciesCode 
ON DataSurveySummary.SpeciesID = MetadataSpeciesCode.SpeciesID)
LEFT OUTER JOIN DataSurveyInformation
ON DataSurveySummary.SurveyID = DataSurveyInformation.SurveyID)
LEFT OUTER JOIN MetadataColonyLocation 
ON DataSurveyInformation.ColonyID = MetadataColonyLocation.ColonyID
WHERE DataSurveySummary.TotalNestCount <> NULL"

dat<-sqlQuery(con, qry)#; head(dat) ##import the queried table

qry<-
  "SELECT MetadataColonyLocation.ColonyName, MetadataSpeciesCode.SpeciesCode, DataAnnualSummary.SurveyYear, DataAnnualSummary.PeakNumberofNests, DataAnnualSummary.PeakNestDate, DataAnnualSummary.PeakNumberOfAdults, DataAnnualSummary.PeakAdultDate, DataAnnualSummary.EstimatedNumberBreeding, DataAnnualSummary.PeakNumberofYoung, DataAnnualSummary.PeakYoungDate
FROM (DataAnnualSummary
LEFT OUTER JOIN MetadataColonyLocation
ON DataAnnualSummary.ColonyID = MetadataColonyLocation.ColonyID)
LEFT OUTER JOIN MetadataSpeciesCode
ON DataAnnualSummary.SpeciesID = MetadataSpeciesCode.SpeciesID"

dat.annual<-sqlQuery(con, qry)#; head(dat) ##import the queried table

##get location info
qry<-
  "SELECT ColonyID, ColonyName, Latitude, Longitude, Datum, Notes
FROM MetadataColonyLocation"

locs<-sqlQuery(con, qry)

##when finished with db, close the connection
odbcCloseAll()

out<-dim(0)
##calculate peak nest counts for 2015 &2017
for (y in 2015:2018) {
  dat.temp<-subset(dat, year==y)
  for (i in 1:length(unique(dat.temp$SpeciesCode))) {
    sp.temp<-unique(dat.temp$SpeciesCode)[i]
    dat.temp2<-subset(dat.temp, SpeciesCode==sp.temp)
    for (j in 1:length(unique(dat.temp2$ColonyName))) {
      colony.temp<-unique(dat.temp2$ColonyName)[j]
      dat.temp3<-subset(dat.temp2, ColonyName==colony.temp)
      if (nrow(dat.temp3)>0){
        ##peak nest
        nest.temp<-dat.temp3$TotalNestCount[which.max(dat.temp3$TotalNestCount)]
        nest.date.temp<-dat.temp3$SurveyDate[which.max(dat.temp3$TotalNestCount)]
        ##adults
        adult.temp<-dat.temp3$TotalAdultCount[which.max(dat.temp3$TotalAdultCount)]
        adult.date.temp<-dat.temp3$SurveyDate[which.max(dat.temp3$TotalAdultCount)]
        ##peak young
        young.temp<-dat.temp3$TotalYoungCount[which.max(dat.temp3$TotalYoungCount)]
        young.date.temp<-dat.temp3$SurveyDate[which.max(dat.temp3$TotalYoungCount)]
        ##replace length 0 with NA
        if (length(nest.temp)==0) {nest.temp<-NA; nest.date.temp<-NA}
        if (length(adult.temp)==0) {adult.temp<-NA; adult.date.temp<-NA}
        if (length(young.temp)==0) {young.temp<-NA; young.date.temp<-NA}
        
        ##assign to dataframe
        out.temp<-data.frame(ColonyName=colony.temp, SpeciesCode=sp.temp, SurveyYear=y, PeakNumberofNests=nest.temp, PeakNestDate=nest.date.temp, PeakNumberOfAdults=adult.temp, PeakAdultDate=adult.date.temp, EstimatedNumberBreeding=nest.temp*2, PeakNumberofYoung=young.temp, PeakYoungDate=young.date.temp)
        if (is.null(out)) {
          out<-out.temp
        } else {
          out<-rbind(out, out.temp)
        }
      }
    }
  }
}

dat.peak<-rbind(dat.annual, out)

##add locations
##add locs to dat.peak
dat.peak.locs<-dplyr::left_join(x=dat.peak, y=subset(locs, select=c(ColonyName, Latitude, Longitude)), by = c("ColonyName","ColonyName"))