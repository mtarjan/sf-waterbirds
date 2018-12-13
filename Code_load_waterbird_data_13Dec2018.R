##load salt pond data
##m tarjan
##December 13, 2018

##LOAD REQUIRED PACAKGES
library(RODBC) ##required to connect to Access database
library(stringr)

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