#R_doppler_v3_checkmarkers
#Some files have markers on channels 7 and 8
#Some have spurious markers

#try change
#This program checks where and when markers occur and sets the correct channel for analysis

setwd("~/doppler_analysis") #this script saved in Git repository

myfileloc<-"~/Dropbox/R/DB_otherscripts/Doppler/" #directory for files
#setwd("c://Users/Alex/Dropbox/Doppler/")

library(xlsx)
require(dplyr) 


#Timings for Freezefoot. The marker occurs at the start of the cartoon
# There is 12s of cartoon, which acts as baseline
# It is followed by ? for talking, then 'shhh' card, during which child stops talking and signal returns to normal
# NB THESE TIMINGS ARE DEFINED RELATIVE TO START OF TALK SIGNAL!!
premarker=-12;#define baseline period in seconds
postmarker=18;#times in seconds defining end of epoch
poistart=4;#period of interest start in secs (ie after ? card)
poiend=14;#period of interest end in secs
baselinecorrect=1
extremehi=140;#define values for rejecting bad epochs
extremelo=60;

#-----------------------------------------------------
#read list of files to analyse
#------------------------------------------------------------------------
xlssummaryfile<-paste(myfileloc,"Twins Doppler_trials for inclusion based on behaviour2.xlsx",sep='')
filelist <- as.data.frame(read.xlsx(xlssummaryfile,sheetName="Sheet1"))
# This is an xls file that has list of files, include/exclude, and flag for each trial
# specifying whether it is included (or excluded because procedural error, 
# such as talking during silent period, or not talking during talk period)
#------------------------------------------------------------------------
# START ANALYSIS (SINGLE FILE VERSION), select and preprocess file
briefinspect<- 0 #set to 1 to inspect markers

########################################################################
for (mysub in 33:35){ #select file here or have loop
########################################################################


myname=paste(myfileloc,filelist[mysub,2],".exp",sep="")

#NB. can double click on exp files to open in word and see header with ID and date/time
dat <- read.table(myname, skip = 6,  header =FALSE, sep ='\t') #read .exp file in to table

wantcols=c(2,7,8) #select columns of interest and put in shortdat: cols for time, marker7 and marker8
shortdat=data.frame(dat[,wantcols])
colnames(shortdat)=c("csec","marker7","marker8")

rawdata=filter(shortdat, row_number() %% 4 == 0) #downsample to 25 Hz by taking every 4th point
allpts=nrow(rawdata) #total N points in long file
rawdata[,1]=(seq(from=1,to=allpts*4,by=4)-1)/100 #create 1st column which is time in seconds from start
colnames(rawdata)=c("sec","marker7","marker8")

#rm(dat,shortdat) #clear original input files from workspace

#attach(rawdata) #makes it easier to use variables from dataframe
#------------------------------------------------------------------------
#brief plot of 12000 pts to check all OK; range here is arbitrary
if (briefinspect==1)
{
  x=rawdata$sec[3000:15000]
  y=rawdata$marker7[3000:15000]
  z=rawdata$marker8[3000:15000]
 
  
  plot(x,y, type="n",main='red:channel 7, blue:channel 8') #set up plot - doesn't actually plot anything
  lines(x,y,col="red") #channel7
  lines(x,z,col="blue") #channel8

 }
#----------------------------------------------------------------------
samplingrate=25
prepoints=premarker*samplingrate
postpoints=postmarker*samplingrate
mylen=nrow(rawdata);
#----------------------------------------------------------------------
#Now find markers; place where go from low to high value
#------------------------------------------------------------------

#markersize=maxmarker-20 #markersize varies with computer? But this should catch all
markersize=80
markerrange=seq(-prepoints:mylen-postpoints);#can't have markers right at start or end
mylen=nrow(rawdata);
for (mychannel in 1:2){
  marker<-marker7
  startcol<-48
  if (mychannel==2){marker<-marker8
  startcol<-54} #information about markers will be written back to columns in xls file
  #in range 48-53 for channel 7 and 54 onwards for channel 8

markerplus=c(0 ,marker);
markerchan=c(marker,0); #create vectors with offset of one
markersub=markerplus-markerchan;#start of marker indicated by large difference in level

maxmarker=max(marker)

origmarkerlist=which(markersub[markerrange]>markersize)
origmarkerlist=origmarkerlist-prepoints;#need to subtract prepoints (-ve value, so add in effect)
# NB this step takes into account that the marker occurs at start of baseline- 
# We therefore adjust so that in effect we are using a marker at a point that is later than this
# (ie when the 'talk' signal appears)


#----------------------------------------------------------------------
# Identify and remove spurious markers
#----------------------------------------------------------------------
#Check that markers are at least 2 s apart
intervals=c(sec[origmarkerlist],10000)-c(0,sec[origmarkerlist])
intervals=intervals[1:(length(origmarkerlist)-1)] #ignore last
#First and last values will be arbitrarily large; others should be around 40 s but may be longer if
#recording interrupted
#Shorter intervals indicate there have been spurious markers

spuriousmarkers=which(intervals<13) #in fact the short ones are the ones we want!
#brief interval  is duration of video
# retain markers with short interval 
if (length(spuriousmarkers)>0){
  spuriousmarkers=c(spuriousmarkers,length(origmarkerlist))
  markerlist=origmarkerlist[spuriousmarkers]#keep first and those with index of spurious marker
}
if (length(spuriousmarkers)==0){markerlist=origmarkerlist}

nmarkers=length(markerlist);
filelist[mysub,startcol]<-nmarkers
filelist[mysub,startcol+1]<-length(spuriousmarkers)
filelist[mysub,startcol+2]<-markerlist[1]
filelist[mysub,startcol+3]<-markerlist[2]
filelist[mysub,startcol+4]<-markerlist[3]
filelist[mysub,startcol+5]<-markerlist[3]-markerlist[2]
}
filelist[mysub,startcol+6]<-filelist[mysub,57]-filelist[mysub,51]
#----------------------------------------------------------------------
}
write.xlsx(filelist, "myfilelist_withmarkers.xlsx",row.names=FALSE)
