#Modeled on Dan Mirman: http://www.danmirman.org/gca

#GCA deals with challenges in analyzing time series data
#Using separate analyses for individual time bins or time windows creates a trade-off 
#   between power (more data in each bin) and temporal resolution (smaller time bins) 
#   and introduces experimenter bias in selection of time bins/windows.
#Statistical thresholding (i.e., p < 0.05 is significant but p > 0.05 is not) 
#    creates false discretization of continuous processes.
#There is no clear way to quantify individual differences, 
#    which are an important source of constraints for theories of cognition.

#----------------------------------------------------------------------------
# check if PC or Mac
#----------------------------------------------------------------------------
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}
#----------------------------------------------------------------------------
  
os1=get_os()
#read background data on twins
dir<-"/Users/dorothybishop/Dropbox/ERCadvanced/project twin kids/Project_files/"
if(os1=="windows"){
  dir<-"/Users/Alex/Dropbox/project twin kids/Project_files/"
}
#----------------------------------------------------------------------------

require(lme4)
require(tidyverse)
require(stargazer) #simple commands for nice tables
library(doBy)
library(Hmisc) #for correlations

#----------------------------------------------------------------------------
file<-"TwinsData_DATA_2017-12-13_1033.csv"
redcapdata<-read.csv(paste0(dir,"Data/",file))
redcapshort<-select(redcapdata,record_id,fam_id,twin,female,zygosity, 
                    dld_rd,include,n_trials,laterality_index,latency,li_se)
redcapshort<-filter(redcapshort,!is.na(laterality_index))
#----------------------------------------------------------------------------
longdata<-read.csv('longrawdopplermeans.csv')
longdata<-filter(longdata,!is.na(value)) #remove missing data
#columns are ID, side, time and value

#take just 30 subjects to check how script works - otherwise too slow
#longdata<-filter(longdata,ID%in%levels(longdata$ID)[1:30])
#POI of 4 to 14
poistart<- 3.99
poiend<-14.01
poidata<-filter(longdata,(time>poistart & time<poiend))
#poidata$value<-scale(poidata$value)


sumpoi<-summaryBy(value ~ side + time, data = poidata,
          FUN = c(mean,sd,length))
# produces mpg.m wt.m mpg.s wt.s for each
# combination of the levels of cyl and vs 
sumpoi$se<-sumpoi$value.sd/sqrt(sumpoi$value.length)



#problems with ggplot
ggplot(sumpoi, aes(value.mean,time, color=side,geom='circle')) +
  labs(y="Fixation Proportion", x="Time since word onset (ms)") + 
  theme_bw() + scale_color_manual(values=c("red", "blue"))
#----------------------------------------------------------------------------
# First example based on Chickdata: nb that is for between subjects whereas
# side is within.

# #"base" model allowing for individual variability in value 
# # (in technical terms, a random intercept for each ID: (1 | ID) )
#   
# m.base <- lmer(value ~ time + (1 |ID), data=poidata, REML=F)
# #----------------------------------------------------------------------------
# #add fixed effect of side on the intercept 
# #(i.e., a constant difference in side )
# m.0 <- lmer(value ~ time + side+(1 |ID), data=poidata, REML=F)
# #----------------------------------------------------------------------------
# #add fixed effect of side on the slope 
# m.1 <- lmer(value ~ time * side+(1 |ID), data=poidata, REML=F)
# #----------------------------------------------------------------------------
# 
# anova(m.base, m.0, m.1)
# coef(summary(m.1))

#----------------------------------------------------------------------------
#in addition to different intercepts, might be other ID effects
#To capture this, add a linear effect on time of ID to the random effects, 
# and we can use model comparisons to examine whether this effect improved model fit:
 # m.t1 <- lmer(value ~ time * side+(1+time|ID), data=poidata, REML=F)
 # anova(m.1, m.t1)
 # coef(summary(m.t1))
 #----------------------------------------------------------------------------
 
 #Need to consider quadratic term
 
 #try doing this first with lm and poly : ignores relationship between subs!
#  m.lm<-lm(value~side*poly(time,2),data=poidata)
#  
# #try it with means from sumpoi
#  m.means<-lm(value.mean~side*poly(time,2),data=sumpoi)
# #can't understand how predict function might give separate prediction for sides: do it by hand to try to work it out!
#  timeseq<-seq(4,14.08,.04)
#  predframe<-data.frame(matrix(NA,ncol=2,nrow=length(timeseq)))
#  for (side in 1:2){
#    thisrow=0
#    for (time in timeseq){
#      thisrow<-thisrow+1
#      predframe[thisrow,side]<-m.means$coefficients[1]+
#        side*m.means$coefficients[2]+
#        poly(timeseq,2)[thisrow,1]*m.means$coefficients[3]+
#        poly(timeseq,2)[thisrow,2]*m.means$coefficients[4]+
#        poly(timeseq,2)[thisrow,1]*side*m.means$coefficients[5]+
#        poly(timeseq,2)[thisrow,2]*side*m.means$coefficients[6]
#    }
#  }
#  #and plot - this works!! - (plot R first so all within Y-axis scale)
# plot(timeseq,predframe[,2])
# lines(timeseq,predframe[,1])
# #Note though that the values from 'predict' are not the same as those I computed!
# # But the values from 'predict' agree much better with raw data.
# 
# #previously used 'lines' here to compare results: they do differ
# #But this plot shows how predict values agree nicely with obs means
# plot(timeseq[1:250],predict(m.means)[253:502],col='blue')
# lines(timeseq[1:250],predict(m.means)[1:250],col='red')
# 
# #superimpose actual means
# wL<-which(sumpoi$side==1)
# wR<-which(sumpoi$side==2)
# lines(timeseq,sumpoi$value.mean[wL],col='red')
# lines(timeseq,sumpoi$value.mean[wR],col='blue')


# create a 2nd order polynomial
# t <- poly((unique(poidata$time)), 2) #create polynomial terms
#poidata[,paste("ot", 1:2, sep="")] <- t[poidata$time, 1:2] #add to poidata
model.poib<-lmer(value~poly(time,2)*side +(1+poly(time,2)*side|ID), data=poidata, REML=F)

#this worked fine until I added side to the random effects!
allranef<-ranef(model.poib)

#Alternative is to look at L-R difference wave, to simplify model.
poiL<-poidata[poidata$side==1,]
poiR<-poidata[poidata$side==2,]
poidiff<-poiL[,c(1,3,4)]
poidiff$value<-poidiff$value-poiR$value

model.poidiff<-lmer(value~poly(time,2) +(1+poly(time,2)|ID), data=poidiff, REML=F)
summary(model.poidiff)
diffranef<-ranef(model.poidiff)

bit<-data.frame(diffranef[[1]]) #turns into data.frame
bit$ID<-row.names(bit)
colnames(bit)<-c('Intercept','linear','quadratic','ID')

alldat<-merge(bit,redcapshort, by.x = "ID", by.y = "record_id")
rdat<-alldat[,c('linear','quadratic','laterality_index')]
rcorr(as.matrix(rdat))
plot(rdat$quadratic,rdat$laterality_index)

#create double entry file
w<-which(colnames(alldat)=='include')
colnames(alldat)[w]<-'myinclude' #to avoid problems with having 'include' as column name
#----------------------------------------------------------
# Produce data frame of selected columns 
#----------------------------------------------------------
data.short<-filter(alldat,zygosity<9)
data.short<-filter(alldat,myinclude==1)
#find solo twins without a pair  reject if fam_id has only one entry
#hideous because fam_id is a factor
famtab<-data.frame(table(data.short$fam_id))
w<-which(famtab$Freq==2)
famtab$fam<-as.numeric(levels(famtab$Var1))
fambit<-famtab$fam[w]
data.short<-filter(data.short,fam_id %in% fambit)
#-------------------------------------------------------
# Create double entry file with twin 1 and 2 aligned
#NB this code needs redoing to cope with possibility of additional columns
# This works but depends crucially on the column numbers specified in the colnames column
#-------------------------------------------------------
nrec<-nrow(data.short)
ncol<-ncol(data.short)
nuorder2<-c(seq(from=2,to=nrec,by=2),seq(from=1,to=nrec,by=2))
nuorder1<-c(seq(from=1,to=nrec,by=2),seq(from=2,to=nrec,by=2))
doubledata<-cbind(data.short[nuorder1,],data.short[nuorder2,])
colrange2<-(ncol+1):(ncol*2)
colnames(doubledata)[colrange2]<-paste0(colnames(doubledata)[colrange2],2)
#check all aligned
check<-sum(doubledata$fam_id-doubledata$fam_id2)
if(check>0) {print('Twins not aligned!!!')}


mztwin<-filter(doubledata,zygosity==1)
dztwin<-filter(doubledata,zygosity>1)
#scatterplot and correlation by zygosity as MZ/DZ
png(filename = "zygolat.png",
    width = 750, height = 400, units = "px", res=80,
    bg = "white")
par(mfrow=c(2,2))
for (i in 1:2){
  mymain='MZ: '
  if (i==2){mymain='DZ: '}
  tempdat <-mztwin
  if(i==2){
    tempdat<-dztwin
  }
  #correlation from double entry gives ICC
  rlat<-cor(tempdat$quadratic,tempdat$quadratic2)
  mysub<-paste0('r = ',toString(round(rlat,3)))
  tempdat1<-filter(tempdat,twin==1)
  mymain=paste0(mymain,'N = ',nrow(tempdat1),' pairs')
  plot(tempdat1$quadratic,tempdat1$quadratic2,
       xlab='quad twin 1',ylab='quad twin 2',main=mymain,cex.lab=1.5,
       cex.main=1.6,xlim=c(-400,400),ylim=c(-400,400))
  text(-200,300,mysub,col='red',font=2,cex=1.4)
  abline(h=0,col=4,lty=2)
  abline(v=0,col=4,lty=2)
  plot(tempdat$laterality_index,tempdat$quadratic,
       ylab='quadratic',xlab='LI',main=mymain,cex.lab=1.5,cex.main=1.6,
       ylim=c(-400,400),xlim=c(-15,10))
  abline(v=0,col=4,lty=2)
}
dev.off()



