##Waterbird Monitoring Methods analysis 2018
##Prepared by M Tarjan
##Prepared for Cheryl Strong, USFWS
##February 5, 2018

library(RODBC)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr) ##required for spread

wb<-"S:/Science/Waterbird/Databases - enter data here!/Cargill Pond Surveys/USGS data from Cheryl 29Jan2018/USGS_SFBBO_pond_data_26Feb2018.accdb"

con<-odbcConnectAccess2007(wb) ##open connection to database

sqlTables(con, tableType="TABLE")$TABLE_NAME ##Get names of available tables

qry<-
  "SELECT d.MonthYear, d.Season, d.YearID, d.CountDate, d.Pond, d.Agency, d.PondGrid, d.SpeciesCode, d.TotalAbundance, s.StandardGuild 
  FROM SBSPBirdData_IncludesNoBirdPondCounts AS d
  LEFT OUTER JOIN SpeciesCodes AS s ON d.SpeciesCode = s.SpeciesCode" 


dat<-sqlQuery(con, qry); head(dat) ##import the queried table

##when finished with db, close the connection
odbcCloseAll()

##use data from sites inside the SBSPRP footprint only
dat.sub<-subset(dat, subset = str_sub(Pond, 1,1) %in% c("A", "B", "R"), select=c(MonthYear, Season, YearID, CountDate, Pond, Agency, PondGrid, SpeciesCode, StandardGuild, TotalAbundance))

##subset to surveys when all ponds were surveyed
#survey.pond.no.occurence<-table(dat.sub$MonthYear, as.character(dat.sub$Pond)) %>% data.frame %>% subset(Freq==0)##ponds/survey combos with no counts
#survey.pond.no.occurence<-survey.pond.no.occurence[-which(survey.pond.no.occurence$Var2 %in% c("A8", "A8S", "A8W", "B10X")),] ##ignore A8 ponds with 0 counts
#dat.complete<-dat.sub[-which(as.character(dat.sub$MonthYear) %in% as.character(survey.pond.no.occurence[,1])),]

##use subset of data until figure out appropriate way to identify complete survey periods
dat.complete<-dat.sub

##Gerrodette. 1987. A power analysis for detecting trends. Ecology.
##calculate statistical power (1-beta) to reject null hypothesis which is false (ie ability to detect increase or decrease in population size, or slope not equal to 0)
##depends on sample size (n), probability of type 1 error (alpha), and magnitude of difference between null hypothesis and reality (effect size, ie rate of change, r); also depends on measurement error (CV, coefficient of variation of abundance estimate, can be estimated by residual variance about the regression line)

dat.complete$year<-format(dat.complete$MonthYear, "%Y")

##add season.year ids, taking into account that winter crosses years
dat.complete$season.yr<-str_c(dat.complete$Season, ".", as.character(dat.complete$year))
dat.complete$season.yr[which(format(dat.complete$MonthYear, "%m") %in% c("01", "02"))]<-str_c(dat.complete$Season[which(format(dat.complete$MonthYear, "%m") %in% c("01", "02"))], ".", as.character(as.numeric(dat.complete$year[which(format(dat.complete$MonthYear, "%m") %in% c("01", "02"))])-1))

##species counts by unique survey
dat.spp<- dat.complete %>% group_by(year, season.yr, MonthYear, Season, SpeciesCode) %>% summarise(abun=sum(TotalAbundance)) %>% data.frame()

##average counts by season and species
dat.season<- dat.spp %>% group_by(Season, season.yr, SpeciesCode) %>% summarise(mean=round(mean(abun),0)) %>% data.frame()

##add ln abun to dat.spp and dat.season
dat.spp$ln.abun<-log(dat.spp$abun+1)
dat.season$ln.mean<-log(dat.season$mean+1)

##plot data for a given species
dat.spp.sub<-subset(dat.spp, SpeciesCode=="DCCO")

##linear model of species over surveys
lm.spp<-lm(formula = ln.abun ~ MonthYear, data = dat.spp.sub)

##ggplot of abundance and linear model
fig <- ggplot(data = dat.spp.sub, aes(x= MonthYear, y=ln.abun))
fig <- fig + geom_point()
fig <- fig + geom_abline(intercept = lm.spp$coefficients[1], slope=lm.spp$coefficients[2])
fig <- fig + geom_line()
fig

##power analysis for time series
##https://stat.ethz.ch/pipermail/r-help/2007-July/136187.html

##Eared grebes
##Action threshold for SBSPRP (Adaptive Management plan): Three consecutive years in which numbers are more than 25% below the NEPA/CEQA baseline, or any single year in which numbers are more than 50% below NEPA/CEQA baseline
##NEPA/CEQA baseline 5630
##counts are mean winter on ponds- Dec,Jan,Feb

##solve for sample size
##sample size to detect a 26% decrease below NEPA baseline in one year with p < 0.05
##assume alpha = beta = 0.05 and change is linear
##r^2 *n(n-1)(n+1)>=156*CV^2 *(1 + (r/2)*(n-1))

##update needed: TAKE THE AVERAGE OF THE MONTHS FOR EACH YEAR. SAMPLE SIZE OF 1 FOR 1 YEAR
#dat.spp.sub<-subset(dat.spp, SpeciesCode=="EAGR" & format(dat.spp$MonthYear, "%m") %in% c("01", "12", "02"))

##linear model of species over surveys
#lm.spp<-lm(formula = abun ~ MonthYear, data = dat.spp.sub)

##loop through months to get mean CV by month
##**assumes variance is based on samples taken within the same month
#out<-dim(0)
#for (j in 1:12) {
#  dat.temp<-subset(dat.spp, SpeciesCode=="EAGR" & format(dat.spp$MonthYear, "%m") == ifelse(j<10, str_c(0,as.character(j)), as.character(j)))
#  if (nrow(dat.temp)==0) {next}
#  lm.temp<-lm(formula = abun ~ MonthYear, data = dat.temp)
#  rmse<-sqrt(mean(lm.temp$residuals^2))
#  CV<-rmse/mean(predict(lm.temp))
#  out<-rbind(out, CV)
#}
#CV.mean<-mean(out)


##residual variance is found by taking the sum of the squares and dividing it by (n-2), where "n" is the number of data points on the scatterplot
#slope<--0.26
#r<-(exp(slope))-1
#rmse<-sqrt(mean(lm.spp$residuals^2))
#CV<-rmse/mean(predict(lm.spp))

#for (n in 1:100) {
#  if ((r^2)*n*(n-1)*(n+1) >= 156*(CV^2)*(1+(r/2)*(n-1))) {
#    print(n); break
#  }
#}

##would need n samples (abundance estimate is average of samples) to detect a rate of change of -0.26 at 0.05 confidence level

##(1-exp(slope))*100 = percent annual decrease; assuming slope is for ln(count)~year 
##see Nur et al 1999 pg 27 for formula to calculate sample size for two-sample t-test

###PWR PACAKGE
#library(pwr)
##u = df of model = # of predictors - 1
##v = df of error = n - p = # observations - # predictors
##for linear model where counts is a function of time, there are two predictors (time and intercept) and number of observations is given by dataset. so u = 1 and v = n - 1
#pwr.f2.test()

##SIMULATION APPROACH
##Wood, J. K., Nur, N., Salas, L., & Richmond, O. M. . (2017). Site-specific Protocol for Monitoring Marsh Birds: Don Edwards San Francisco Bay and San Pablo Bay National Wildlife Refuges. Prepared for the U.S. Fish and Wildlife Service, Pacific Southwest Region Refuge Inventory and Monitoring Initiative. Petaluma, CA. Retrieved from file:///C:/Users/Marla/Downloads/SF_Bay_Marsh_Bird_Protocol_FINAL_20170208 (1).pdf

##check if error is related to mean count
#dat.temp<-dat.spp %>% group_by(season.yr, SpeciesCode) %>% summarise(sd=sd(abun), mean=mean(abun)) %>% data.frame()
#plot(sd~mean, data=dat.temp)

pulse<-F
rep<-1000
percents<-c(-10, -15, -20, -50)
years<-3:15
season.n<-2 ##number of samples taken within the season/year
#spp<-unique(dat$SpeciesCode)
spp<-c("RUDU", "BUFF", "CANV", "SCAU", "WESA", "LESA", "EAGR", "PHAL", "RNPH", "BOGU", "NSHO", "WILL", "AMAV", "MAGO", "LETE", "FOTE", "CATE")
#spp<-c("WESA", "LESA", "EAGR", "PHAL", "RNPH", "BOGU", "NSHO", "WILL", "AMAV", "MAGO", "LETE", "FOTE", "CATE")
#out.sim<-data.frame(sp=NA, rep=NA, per=NA, season.n=NA, season=NA, years=NA, detect=NA, count=NA)
power.dat.out<-dim(0)
for (j in 1:length(spp)) { ##for each species
  out.sim<-data.frame(sp=NA, rep=NA, per=NA, season.n=NA, season=NA, years=NA, detect=NA, count=NA)
  
  spp.temp<-spp[j] ##species to analyze
  print(spp.temp)
  ##estimate error from historical data
    #dat.temp<-subset(dat.spp, SpeciesCode==spp.temp) ##total count of that species for the study area for each monthyear (ie survey period). assumes that only used data for surveys with complete counts across all ponds (see above code)
    ##find months with max counts
    #dat.temp$month<-as.numeric(format(dat.temp$MonthYear, "%m")) ##add month
    #dat.month<- dat.temp %>% group_by(month) %>% summarise(mean=mean(abun)) %>% data.frame() ##get mean count by pond and month (across all data years)
    #dat.month <- dat.month[order(dat.month$mean, decreasing = T),]
    #months<-dat.month$month[1:3] ##select 3 months with highest mean counts
    ##should there be a requirement that months are sequential? top month and the months before and after?
    #dat.temp<-subset(dat.temp, month %in% months) ##subset data to only include months with highest counts
    
    #alternatively use seasonal data
    dat.temp<-subset(dat.season, SpeciesCode==spp.temp)
    dat.season.mean<-dat.temp %>% group_by(Season) %>% summarize(mean=mean(mean)) %>% data.frame()
    season<-as.character(dat.season.mean$Season[which.max(dat.season.mean$mean)])
    
    dat.temp<-subset(dat.spp, Season==season & SpeciesCode==spp.temp)
    
    ##visualize data and error
    #fig <- ggplot(data = dat.temp, aes(x = MonthYear, y = abun))
    #fig <- fig + geom_point()
    #fig <- fig + geom_path()
    #fig <- fig + geom_smooth(method="lm")
    #fig
    
    ##calculate error around lm
    #lm.temp<-lm(ln.abun~MonthYear, data=dat.temp)
    #rmse<-sqrt(mean(lm.temp$residuals^2)) ##root mean squared error
    #variance<-sum(lm.temp$residuals^2)/(nrow(dat.temp)-1) #variance
    #CV<-rmse/mean(predict(lm.temp)) ##coef of variation
    
    ##calculate within year error
    error.yr<-dat.temp %>% group_by(season.yr) %>% summarise(sd=sd(abun), mean=mean(abun)) %>% data.frame()
    sd.yr<-mean(error.yr$sd, na.rm=T)
    variance<-sd.yr^2
    
    if (is.na(variance)) {next} ##if the variance in NA, then this species has only been surveyed once in nay given year and it won't be possible to estimate error. move to the next species
  
  ##vary the percent annual change
    for (c in 1:length(percents)) { ##for percent annual change -10%, -20%, etc
    per.temp<-percents[c] ##percent annual change
    for (r in 1:rep) { ##for rep replicates
      ##simulate 20 years of data with error from the historical data and certain % annual change
      ##note that simulated abundance actually represents simulated count data. abundance is the assumed true abundance
      sim.dat<-data.frame(year=rep(1:20, season.n))
      for (i in 1:length(sim.dat$year)) {
        if (sim.dat$year[i]==1) {
          sim.dat$abun[i]<-round(mean(dat.temp$abun),0) ##starting abundance
        } else {
          #sim.dat$abun[i]<-round(subset(sim.dat, year==sim.dat$year[i]-1)$abun[1] + subset(sim.dat, year==sim.dat$year[i]-1)$abun[1]*per.temp/100,0)
          ##change this so it is an exponential model
          N0<-round(mean(dat.temp$abun),0)
          r.temp<-log(1+per.temp/100)
          t<-sim.dat$year[i]-1
          sim.dat$abun[i]<-N0*exp(r.temp*t)
          
          ##overwrite years 2-t with pulse change (given percent decrease in year 2 that continues for subsequent years)
          if(pulse==T) {
            sim.dat$abun[i]<-N0*(1+per.temp/100)
          }
        }
        
        ##draw from distribution with min=0, mean=abundance, spread equal to variance in our dataset
        #count.distrib<-rnorm(mean=sim.dat$abun[i], sd=rmse, n = 10000)
        #count.distrib<-count.distrib[which(count.distrib >=0)]
        ##use poisson or negative binomial instead
        ##mu = mean, size=dispersion parameter, where variance = mu +mu^2/size
        mu <- sim.dat$abun[i]
        variance <- variance
        size <- (mu + mu^2) / variance
        sd.lm<-lm(sd~ 0 + mean, data=error.yr) ##forces lm through the origin
        sd.corrected<-mu*coefficients(sd.lm)
        #count.distrib<-rnbinom(n = 10000, mu = mu, size = size)
        #count.distrib<-rpois(n = 10000, lambda = mu) ##lambda is the mean
        count.distrib<-round(rnorm(n = 10000, mean = mu, sd = sd.corrected),0) ##draw from normal distrib
        
        
        #count.distrib<-subset(count.distrib, count.distrib>=0) ##remove negative values for annual percent decline case
        count.distrib<-replace(count.distrib, which(count.distrib<0), 0) ##replace negative values with 0
        
        #hist(count.distrib)
        sim.dat$abun.sim[i]<-sample(count.distrib, size = 1)
      }
      
      #plot(log(abun+1)~year, data=sim.dat); points(log(abun.sim+1)~year, data=sim.dat, col="blue")
      
      for (y in years) { ##for 5 to 20 years of data
        ##subset data to selected years
        sim.dat.sub<-subset(sim.dat, year<=y)
        ##count in last year
        last.count<-sim.dat.sub$abun.sim[which.max(sim.dat.sub$year)]
        ##create linear model of log-linearized abundances
        lm.temp<-lm(log(abun.sim+1)~year, data=sim.dat.sub)
        
        ##for pulse change, run t-test of first year compared to subsequent years
        ttest.temp<- t.test(x = subset(sim.dat.sub, year>1)$abun.sim, mu= sim.dat.sub$abun[1], alternative="less")
        
        ##report whether or not trend was detected
        ifelse(coefficients(lm.temp)[2]<0 & summary(lm.temp)$coefficients[2,4] <=0.05, detect.temp<-"yes", detect.temp<-"no") ##this just requires that the trend is negative, but might want to have requirements about the magnitude (ie slope must be similar to given percent decrease, including error in estimate)
        
        ##overwrite yes/no determinant with ttest output if testing pulse case
        if (pulse==T) {
          ifelse(ttest.temp$p.value <=0.05, detect.temp<-"yes", detect.temp<-"no")
          ##calculate the percent change across the interval
          #y1<-exp(predict(object = lm.temp, newdata = data.frame(year=1), interval = "confidence"))-1
          #ymax<-exp(predict(object = lm.temp, newdata = data.frame(year=y), interval = "confidence"))-1
          #change.temp<-(ymax-y1)/y1*100
          #ifelse(coefficients(lm.temp)[2]<0 & summary(lm.temp)$coefficients[2,4] <=0.05 & change.temp[2]<=per.temp, detect.temp<-"yes", detect.temp<-"no")
          
          #if (detect.temp=="yes") {print(change.temp)}
        }
        
        out.sim<-rbind(out.sim, data.frame(sp=spp.temp, rep=r, per=per.temp, season.n=season.n, season=season, years=y, detect=detect.temp, count=last.count)) ##rep, per.temp, years studied, trend detected?
      } ##end loop through years
      
      if (r==1) {
        fig <- ggplot(data = sim.dat, aes(x=year))
        fig <- fig + geom_point(aes(y=abun.sim))
        fig <- fig + geom_line(aes(y=abun))
        fig <- fig + ggtitle(str_c(spp.temp, " at ", per.temp, "% decline"))
        #fig <- fig + ggtitle(str_c(spp.temp, " at ", per.temp, "% decline;  lwr CI ", round(change.temp[2], 2), "%"))
        #fig <- fig + geom_abline(slope = coefficients(lm.temp)[2], intercept = coefficients(lm.temp)[1], color="blue")
        fig
        print(fig)
      }
      
    } ##end loop through reps
  } ## end loop through percent annual change
    
    out.sim<-out.sim[complete.cases(out.sim),] ##get rid of first row of NAs
    
    ##calculate power to detect trend
    power.dat<-out.sim %>% group_by(sp, per, season.n, season, years, detect) %>% count(detect) %>% data.frame ##get counts of yes' and no's for detections
    
    power.count<-out.sim %>% group_by(sp, per, season.n, season, years) %>% summarise(count=round(mean(count),0)) %>% data.frame ##get mean count for that category
    
    power.dat<-dplyr::left_join(x=power.dat, y=power.count, by = NULL)
    
    ##spread detections
    power.dat<-spread(power.dat, detect, n, fill = 0)
    if (is.null(power.dat$no)) {power.dat$no<-0}
    if (is.null(power.dat$yes)) {power.dat$yes<-0}
    ##add column with % positive detections
    power.dat$power<-power.dat$yes/(power.dat$yes+power.dat$no)
    
    power.dat.out<-rbind(power.dat.out, power.dat)
    
} ##end loop through species

##plot one example of simulated data
#plot(log(abun.sim+1)~year, data=sim.dat.sub, main=per.temp); abline(coefficients(lm.temp)[1], coefficients(lm.temp)[2]); text(x=15, y=2, labels = round(summary(lm.temp)$coefficients[2,4], 2))

power.dat<-power.dat.out

write.csv(power.dat, "power.dat.csv", row.names = F)

##make table of survey years until >0.8 power
power.table<-power.dat %>% group_by(sp, per, season.n) %>% subset(power > 0.8) %>% data.frame()
out<-power.table[1,]
for (j in 1:nrow(unique(subset(power.table, select=c(sp, per, season.n))))) {
  group.temp<-unique(subset(power.table, select=c(sp, per, season.n)))[j,]
  group.temp<-subset(power.table, sp==group.temp$sp & per==group.temp$per & season.n==group.temp$season.n)
  out.temp<-group.temp[which.min(group.temp$years),]
  out<-rbind(out, out.temp)
}
out<-unique(out)
power.table<-subset(out, select=-c(no, yes))

write.csv(power.table, "power.table.csv", row.names = F)

power.table.spread<-subset(power.table, select= c(sp, season, per, season.n, years)) %>% spread(key = season.n, value = years)
power.table.spread<-replace(power.table.spread, list = is.na(power.table.spread), values = ">15")

write.csv(power.table.spread, "power.table.spread.csv", row.names=F)

##plot results
n.plot<-1 ##number of seasons to plot
for (j in 1:length(unique(power.dat$sp))) {
  data.plot<-subset(power.dat, sp==unique(power.dat$sp)[j] & season.n==n.plot)
  
  spp.temp<-unique(power.dat$sp)[j]
  season.n.temp<-unique(data.plot$season.n)
  season.temp<-unique(data.plot$season)
  
  fig <- ggplot(data = data.plot, aes(x=years, y=power, color=factor(per)))
  fig <- fig + geom_point() + geom_path()
  fig <- fig + geom_abline(slope = 0, intercept = 0.8, lty="dashed")
  fig <- fig + theme_classic()
  fig <- fig + theme(text = element_text(size=16))
  fig <- fig + labs(color=ifelse(pulse==T, "Percent \npulse change","Percent \nannual change"))
  fig <- fig + xlab("Number of years") + ylab("Power to detect trend")
  fig <- fig + ggtitle(str_c(spp.temp, " with ", season.n.temp, " ",season.temp," survey(s)"))
  #fig <- fig + ggtitle(spp.temp)
  fig <- fig + scale_x_continuous(breaks = years, labels=years)
  fig <- fig + scale_y_continuous(breaks = seq(0,1,0.1), labels=seq(0,1,0.1))
  fig
  
  png(filename = str_c("fig.",spp.temp,".",season.n.temp, "surveys.png"), units="in", width=6.5, height=3.5,  res=200);print(fig); dev.off()
}
fig
