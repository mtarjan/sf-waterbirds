##Waterbird Monitoring Methods analysis 2018
##Prepared by M Tarjan
##Prepared for Cheryl Strong, USFWS
##February 5, 2018

#library(RODBC)
library(dplyr)
library(ggplot2)
#library(stringr)
library(tidyr) ##required for spread

##CREATE FOLDER FOR FIGURES
dir.create(str_c("figures.", Sys.Date()))
file.path<-str_c("figures.", Sys.Date())

if (exists("dat.complete")==F) {
  source('Code_load_waterbird_data_13Dec2018.R')
}
head(dat.complete)

##LOAD POND CATEGORY DATA (EG BREACHED, MANAGED)
pond.cat<-read.csv("S:/Science/Waterbird/Program Folders (Gulls, SNPL, ADPP, etc)/Cargill Pond Surveys/Reports/2019 Waterbird Trend Assessment/SBSP pond categories.csv")
wish.list<-c("RSF2U1", "RSF2U2", "RSF2U3", "RSF2U4", "A16", "A17", "A19", "A8", "E12", "E13", "E9", "E8A", "E10", "R3", "R4", "A1", "E6", "E6C", "E4C", "E5C") ##list of ponds that managers want to include in subset based on meeting with PMT in Feb 2019

##SUBSET SURVEY SITES

##CREATE SET OF SURVEY SITES WITH SIMILAR COMPOSITION TO ENTIRE SET
##bioenv in vegan package
##dimensions of matrix are species. multiple matrices for multiple sites. dimensions will be guilds
library(vegan)
##http://menugget.blogspot.com/2011/06/clarke-and-ainsworths-bioenv-and-bvstep.html
##comm matrix row = sites & columns = species. obviously need to be in same order
##bioenv(comm = , env =); gives Subset of environmental variables (column names in env) with best correlation to community data.
##set comm = entire set of sites and env = the same so will test subsets of the total. will give set of sites with best match to overall data. output is set of column names. so the column names need to be the sites. in that case, both matrices are the same. columns are sites and rows are species...
##but pete said the matrix is a similarity matrix that is sp by sp. but maybe that's what the program creates in the background.
source("Code bvstep function.R")

guildxsite<-subset(dat.complete, StandardGuild %in% c("HERON", "MEDSHORE","FISHEAT","DABBLER","DIVER","GULL","TERN","EAREDGR","SMSHORE","PHAL") & str_sub(Pond, 1,1) %in% c("A", "B", "R")) %>% group_by(MonthYear, Pond, StandardGuild) %>% summarise(abun=sum(TotalAbundance)) %>% data.frame() %>% group_by(Pond, StandardGuild) %>% summarise(av.abun=mean(abun, na.rm=T)) %>% data.frame() %>% spread(key = Pond, value = av.abun, fill = 0) #rows are guilds and columns are sites
guildxsite<-subset(guildxsite, select=-StandardGuild) ##remove the guild name

#sets <- bioenv(comm = guildxsite[,1:15], env = guildxsite[,1:15]) ##require any kind of standardizing transformation?

sets <- bv.step(guildxsite, guildxsite,  
                              fix.dist.method="bray", var.dist.method="bray", 
                              scale.fix=FALSE, scale.var=FALSE,  
                              max.rho=0.995, min.delta.rho=0.001, 
                              random.selection=TRUE, 
                              prop.selected.var=0.3, 
                              num.restarts=50, 
                              output.best=10, 
                              var.always.include=NULL) 
sets$order.by.best
sets$order.by.i.comb
sets$var.always.include
sets$var.exclude
top.var<-as.numeric(str_split(sets$order.by.i.comb$var.incl[3], pattern = ",")[[1]])

#second round 
##update variables to always include based on round above
sets2  <- bv.step(guildxsite, guildxsite,  
                                fix.dist.method="bray", var.dist.method="bray", 
                                scale.fix=FALSE, scale.var=FALSE,  
                                max.rho=0.995, min.delta.rho=0.001, 
                                random.selection=TRUE, 
                                prop.selected.var=0.3, 
                                num.restarts=50, 
                                output.best=10, 
                                var.always.include=top.var)
sets2$order.by.best
sets2$order.by.i.comb
sets2$var.always.include
sets2$var.exclude

##see which variables are best subset
#colnames(guildxsite)[c(4,10,16,18,23,24,26,37,46,53,54)]
set.select<-colnames(guildxsite)[as.numeric(str_split(sets2$order.by.best$var.incl[1], pattern = ",")[[1]])]

##ADD PONDS BASED ON WISH LIST FROM PROJECT MANAGEMENT TEAM
set.select2<-unique(c(set.select, wish.list))

##ADD PONDS USING WEIGHTED RANDOM SELECTION BASED ON GUILD/SPECIES ABUNDANCE

##species counts by survey (i.e. monthyear) and pond
dat.pond<-dat.complete %>% group_by(year, season.yr, MonthYear, Season, Pond, StandardGuild) %>% summarise(abun=sum(TotalAbundance)) %>% data.frame()

##take the average abundance by pond and species across all surveys
dat.pond.av<-dat.pond %>% group_by(Pond, StandardGuild) %>% summarise(abun=mean(abun)) %>% data.frame()

##select representative historical abundances by pond. for now just use one survey and a few species. eventually need to do this by guild
speciesOI<-c("SMSHORE", "DABBLER", "DIVER", "EAREDGR", "FISHEAT", "TERN") ##species of interest
dat.pond.av<-subset(dat.pond.av, StandardGuild %in% speciesOI)

#sub.n<-round(length(levels(dat.pond$Pond))*0.5/length(unique(dat.pond$StandardGuild)),0) ##number of ponds to sample for each species = total number of ponds * fraction of ponds to survey / number of species
sub.n<-length(unique(dat.complete$Pond))*1 ##total number of ponds to sample using this protocol = total ponds * percent of ponds for subset

ponds.subset.ordered<-set.select2
for (j in 1:1000) {
  sp.temp<-unique(dat.pond.av$StandardGuild)[sample(x = 1:length(unique(dat.pond.av$StandardGuild)), 1)]
  dat.temp<-subset(dat.pond.av, StandardGuild==sp.temp)
  ##natural log-total as the continuous stratification weight to even the sample selection probabilities (Wood et al 2010)
  dat.temp$ln.abun<-log(dat.temp$abun)
  sample.temp<-as.character(sample(dat.temp$Pond, size=1, replace=F, prob=dat.temp$ln.abun)) ##randomly sample "top" ponds for that species
  ##add it to the top for the other species
  ponds.subset.ordered<-unique(c(ponds.subset.ordered, sample.temp))
  if (length(ponds.subset.ordered)>=sub.n) {break} ##break out of loop once enough ponds have been selected
}
ponds.subset.ordered ##get all ponds in order of preference for subset


##ASSESSMENT OF SUBSET
frac.sub<-c(0.3, 0.4, 0.5, 0.7, 1) ##fraction of ponds to survey

##create a map of subsets
library(rgdal)
land<- rgdal::readOGR(dsn = "S:/Science/GIS/SaltPonds_fromUGSS/shapefiles", layer= "mbyasfby_NAD27")
CA<-rgdal::readOGR(dsn = "S:/Science/GIS/Salt Pond", layer= "CA_boundary_NAD83")
ponds.poly<-rgdal::readOGR(dsn = "S:/Science/GIS/Salt Pond/all_salt_pond_grids_2014", layer= "2014_11_19_pond_scale")

##example
#https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf
#plot(lnd, col = "lightgrey") # plot the london_sport object
#sel <- lnd$Partic_Per > 25
#plot(lnd[ sel, ], col = "turquoise", add = TRUE) # add selected zones to map

plot(ponds.poly)
plot(CA, col="lightgrey")

##ASSESSMENT 1: ALLIGNMENT OF SUBSET COUNTS WITH OVERALL COUNTS
##check which years have counts for all ponds
table(dat.pond$season.yr, dat.pond$Pond)

out<-dim(0)
for (f in 1:length(frac.sub)) {
  for (j in 1:length(speciesOI)) { ##for each species of interest
    species.temp<-speciesOI[j]
    counts.temp<-subset(dat.pond, StandardGuild==species.temp & Pond %in% ponds.subset.ordered[1:round(length(ponds.subset.ordered)*frac.sub[f], 0)]) %>% group_by(MonthYear, year, Season) %>% summarise(count=sum(abun)) %>% data.frame() ##take the counts for that species from the subset sites
    counts.temp$Guild<-species.temp
    counts.temp$pond.fraction<-frac.sub[f]
    out<-rbind(out, counts.temp)
  }
}
out[is.na(out)]<-0


##compare trends of counts in all ponds to trends in counts from subset of ponds
##note that some ponds may not have been counted in all survey periods

fig <- ggplot(data = subset(out, year >= 2005 & Season %in% c("Fall", "Winter")), aes(x = MonthYear, y = log(count+1), color=factor(pond.fraction)))
fig <- fig + geom_point()
fig <- fig + geom_smooth(method = "loess", se = F)
fig <- fig + facet_wrap(facets = ~Guild, scales="free")
fig <- fig + xlab("Date") + ylab("ln(Bird count)")
fig <- fig + theme_classic()
fig <- fig + theme(strip.background = element_rect(colour = "white", fill = "white"))
fig <- fig + scale_y_continuous(breaks = function(x) round(seq(from = x[1],to = x[2],by = (x[2]-x[1])/10),2))
fig <- fig + scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")
fig <- fig + theme(axis.text.x = element_text(angle = 45, hjust=1, color="black"), axis.text.y = element_text(color="black"))
fig <- fig + labs(color = "Proportion \nof sites")
fig

fig.loess<-fig

##test whether trends are different for all data versus pond subset
slopes<-dim(0)
for (f in 1:length(frac.sub)) {
  for (j in 1:length(unique(out$Guild))) {
    guild.temp<-unique(out$Guild)[j]
    dat.temp<-subset(out, Guild==guild.temp & year >=2005 & pond.fraction==frac.sub[f])
    lm.all<-lm(formula = log(count+1) ~ MonthYear, data = dat.temp)
    slopes<-rbind(slopes, data.frame(Guild=guild.temp, pond.fraction=frac.sub[f], Slope= summary(lm.all)$coefficients[2,1], se = summary(lm.all)$coefficients[2,2]))
  }
}

##plot slopes
fig <- ggplot(data = slopes, aes(x = Guild, y = Slope, color = factor(pond.fraction)))
fig <- fig + geom_point(position= position_dodge(width = 0.5), size=3)
fig <- fig + geom_errorbar(aes(ymin=Slope-se, ymax=Slope+se), size = 1.05, width=.5, position = position_dodge(width = 0.5))
fig <- fig + ylab("Slope of ln(Count) ~ Date")
fig <- fig + theme_classic()
#fig <- fig + scale_y_continuous(breaks = function(x) seq(from = x[1],to = x[2],by = abs(x[2]-x[1])/10))
fig <- fig + labs(color = "Proportion \nof sites")
fig

fig.slope<-fig

#<-data.frame(MonthYear= rep(out.spread$MonthYear[945:946], 2), pondgroup=c("count", "count", "count.sub", "count.sub"))
#dat.test$pred<-predict(object = lm.temp, newdata = dat.test)
#dat.test$se<-predict(object = lm.temp, newdata = dat.test, se.fit = T)$se.fit

##ASSESSMENT 2: TEST POWER TO DETECT TRENDS WITH SUBSET OF PONDS AND/OR REDUCED SURVEY FREQUENCY

##Gerrodette. 1987. A power analysis for detecting trends. Ecology.
##calculate statistical power (1-beta) to reject null hypothesis which is false (ie ability to detect increase or decrease in population size, or slope not equal to 0)
##depends on sample size (n), probability of type 1 error (alpha), and magnitude of difference between null hypothesis and reality (effect size, ie rate of change, r); also depends on measurement error (CV, coefficient of variation of abundance estimate, can be estimated by residual variance about the regression line)

##species counts by unique survey
dat.spp<- subset(dat.complete, Pond %in% ponds.subset.ordered[1:(round(85*frac.sub, 0))]) %>% group_by(year, season.yr, MonthYear, Season, SpeciesCode) %>% summarise(abun=sum(TotalAbundance)) %>% data.frame()

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

pulse<-T
rep<-1000
percents<-c(-10, -15, -20, -50)
years<-3:15
season.n<-2 ##number of samples taken within the season/year
#spp<-unique(dat$SpeciesCode)
spp<-c("RUDU", "BUFF", "CANV", "SCAU", "WESA", "LESA", "EAGR", "PHAL", "RNPH", "BOGU", "NSHO", "WILL", "AMAV", "MAGO", "LETE", "FOTE", "CATE")
#spp<-c("WESA", "NSHO", "WILL")
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
n.plot<-2 ##number of seasons to plot
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
  
  png(filename = str_c(file.path, "/fig.",spp.temp,".",season.n.temp, "surveys.png"), units="in", width=6.5, height=3.5,  res=200);print(fig); dev.off()
}
fig

##make a table comparing the power analyses from the full set of sites to the subset of sites
##load output from power analyses of full set
power.table.spread.full<-read.csv("S:/Science/Waterbird/Program Folders (Gulls, SNPL, ADPP, etc)/Cargill Pond Surveys/Reports/2018 Methods Analysis/Waterbird Monitoring Methods Analysis 2018/power.table.spread.pulse.12Apr2018.csv")
power.fullset<-subset(power.table.spread.full, select=-c(X3))
colnames(power.fullset)<-c("sp", "season", "per", "All Sites 1 survey", "All Sites 2 surveys")
colnames(power.table.spread)<-c("sp", "season", "per", "Subset of Sites")
library(dplyr)
power.table.compare<-dplyr::full_join(subset(power.fullset, select = -season), subset(power.table.spread, select= -season))
power.table.compare$`Subset of Sites`[which(is.na(power.table.compare$`Subset of Sites`))]<-">15"
write.csv(power.table.compare, "power.table.freq.v.subset.csv", row.names=F)

##ASSESS EFFECT OF REMOVING GRIDDING ON SURVEY DURATION
##remove durations with error (duration is negative)
dat.sfbbo$grids<-T
dat.sfbbo$grids[which(dat.sfbbo$year==2019)]<-F
dat.sub<-subset(dat.sfbbo, duration.mins>1)
dat.sub<-unique(subset(dat.sub, month %in% c("01", "02") & Pond %in% unique(dat.sub$Pond[which(dat.sub$year==2019 & dat.sub$month=="01")]), select= c(Date, Pond, year, month, duration.mins, grids))) ##get unique records of pond, date, and survey duration
head(dat.sub)

##remove unused ponds
dat.sub<-droplevels(dat.sub)

##order pond facets by duration values
dat.sub<-dat.sub[order(dat.sub$duration.mins, decreasing = T),]
#Turn your 'treatment' column into a character vector
dat.sub$Pond <- as.character(dat.sub$Pond)
#Then turn it back into a factor with the levels in the correct order
dat.sub$Pond <- factor(dat.sub$Pond, levels=unique(dat.sub$Pond))

#boxplot(dat.sub$duration.mins[which(dat.sub$grids==T)]~dat.sub$Pond[which(dat.sub$grids==T)], ylab="Survey duration (mins)")
#points(x=dat.sub$Pond[which(dat.sub$grids==F)], y = dat.sub$duration.mins[which(dat.sub$grids==F)], col="red", pch=4)

#boxplot(dat.sub$duration.mins~dat.sub$grids)

library(ggplot2)
fig <- ggplot(data = dat.sub, aes(x = Pond, y = duration.mins, colour=grids))
fig <- fig + geom_boxplot(data = subset(dat.sub, grids==T))
fig <- fig + geom_boxplot(data = subset(dat.sub, grids==F))
fig <- fig + ylab("Survey duration (mins)")
fig <- fig + theme_classic()
fig <- fig + scale_y_continuous(breaks = seq(0,max(dat.sub$duration.mins), 25))
fig <- fig + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1))
fig

fig.grid.effect<-fig

png(filename = str_c(file.path, "/fig.grid.effect.png"), units="in", width=8, height=5,  res=200);print(fig); dev.off()

##test it
duration.test<-t.test(duration.mins~grids, data= dat.sub)
