#R_doppler_analyse

#Because this program waits for input from user, it only runs properly if you run from source, 
# which you can do with Ctrl+Shift+S

#To Do: allow user to override automated artrej in first stage in either direciton
# Make rejection codes more transparent

# This version checks both marker channels (7 and 8) to ensure we have the correct marker channel in analysis
# NB some files have odd markers: either in both channels, 12 s apart (i.e. one for
# start of video and one for start of talk signal);
# Some have two markers, one in channel 7 and one in channel 8

# Also there is often a spurious marker at start up that needs to be excluded;
# This can be detected by timings

#setwd("C:/Users/dbishop/Dropbox/R/DB_otherscripts/doppler") #set working directory
filedir<-"~/Dropbox/R/DB_otherscripts/Doppler/" #setting directory for files
setwd("~/Doppler_analysis")
#setwd("C:/Users/wilsona/Dropbox/Doppler")

#install.packages(c("readxl","dplyr"))
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


#------------------------------------------------------------------
# Toggle these initialdatacheck values to view aspects of analysis
# Once satisfied, set all to zero for fast analysis
#------------------------------------------------------------------
briefinspect=1; #set to 1 to see a sample of the file to check markers etc are there
initialdatacheck=1; #set to 1 to view raw data for each epoch
initialdatacheck1=0; # set to 1 to view epochs after normalisation
initialdatacheck2=0; #set to 1 to view epochs after heartbeat Correction
initialdatacheck3=0; # set to 1 to visualise after baseline correction
initialdatacheck4=1; # set to 1 to plot average for each subject

#-----------------------------------------------------
#read list of files to analyse
#------------------------------------------------------------------------
xlsfile<-"myfilelist_R2.xlsx"
xlsfilepath<-paste(filedir,xlsfile,sep='')

filelist <- read.xlsx(xlsfilepath,sheetName="Sheet1")
# This is an xls file that has list of files, include/exclude, and flag for each trial
# specifying whether it is included (or excluded because procedural error, 
# such as talking during silent period, or not talking during talk period)
#------------------------------------------------------------------------
# START ANALYSIS (SINGLE FILE VERSION), select and preprocess file


#######################################################################
mysub=26

#select file here or have loop
########################################################################

mysubname<-filelist[mysub,2]
myname=paste(filedir,mysubname,".exp",sep="")

#NB. can double click on exp files to open in word and see header with ID and date/time
dat <- read.table(myname, skip = 6,  header =FALSE, sep ='\t') #read .exp file in to table

wantcols=c(2,3,4,7,8) #csec, L, R,markers #select columns of interest and put in shortdat
shortdat=data.frame(dat[,wantcols])
colnames(shortdat)=c("csec","L","R","marker1","marker2")

rawdata=filter(shortdat, row_number() %% 4 == 0) #downsample to 25 Hz by taking every 4th point
allpts=nrow(rawdata) #total N points in long file
rawdata[,1]=(seq(from=1,to=allpts*4,by=4)-1)/100 #create 1st column which is time in seconds from start
colnames(rawdata)=c("sec","L","R","marker1","marker2")

#rm(dat,shortdat) #clear original input files from workspace

attach(rawdata) #makes it easier to use variables from dataframe
#------------------------------------------------------------------------
#brief plot of first 3000 pts to check all OK; range here is arbitrary
if (briefinspect==1)
{
  x=sec[1:3000]
  y=L[1:3000]
  z=R[1:3000]
  w1=marker1[1:3000]
  w2=marker2[1:3000]
  plot(x,y, type="n") #set up plot - doesn't actually plot anything
  lines(x,y,col="red")
  lines(x,z,col="lightblue")
  lines(x,w1)
  lines(x,w2,lty=4)
  text(50,50,'chan 7 marker black, chan 8 marker dotted')
  
  line <- readline()
  #This should show left (red) and right (blue) channels and some markers in black
  #Both marker channels now shown
  #There is often a spurious marker right at the start
  }
#----------------------------------------------------------------------
samplingrate=25
prepoints=premarker*samplingrate
postpoints=postmarker*samplingrate
poistartpoints=poistart*samplingrate
poiendpoints=poiend*samplingrate
basepoints=1:-prepoints; #baseline is interval prior to marker
maxtrials=30; #needed to set dim for storing rejected epochs

----------------------------------------------------------------------
#Now find markers; place where go from low to high value
#------------------------------------------------------------------

#markersize=maxmarker-20 #markersize varies with computer? But this should catch all
markersize=10 #previously set to 80; varies a fair bit, but should not get spurious activity
#with this setting, as background variation is below 1
mylen=nrow(rawdata)
markerrange=seq(-prepoints:mylen-postpoints);#can't have markers right at start or end
allmarkerlist<-data.frame(matrix(rep(NA,124),ncol=2)) #will hold markers from both channels, 1 col denotes channel, and other is latency
thisoffset=0
for (mychannel in 1:2){
  marker<-marker1
  startcol<-48
  if (mychannel==2){marker<-marker2
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
  
   nmarkers=length(origmarkerlist);
  filelist[mysub,startcol]<-nmarkers
  filelist[mysub,startcol+1]<-length(spuriousmarkers)
  filelist[mysub,startcol+2]<-origmarkerlist[1]
  filelist[mysub,startcol+3]<-origmarkerlist[2]
  filelist[mysub,startcol+4]<-origmarkerlist[3]
  filelist[mysub,startcol+5]<-origmarkerlist[3]-origmarkerlist[2]
  thislen<-length(markerlist)
  allmarkerlist[(1+thisoffset):(thisoffset+nmarkers),1]<-mychannel #record the channel of origin
  allmarkerlist[(1+thisoffset):(thisoffset+nmarkers),2]<-origmarkerlist #record latency of marker
  thisoffset<-thisoffset+nmarkers
}
filelist[mysub,startcol+6]<-filelist[mysub,57]-filelist[mysub,51]
allmarkerlist <- allmarkerlist[order(allmarkerlist$X2),] 
#----------------------------------------------------------------------
#----------------------------------------------------------------------
# Identify and remove spurious markers
#----------------------------------------------------------------------
#Check that markers are at least 2 s apart
intervals=c(sec[allmarkerlist$X2],10000)-c(0,sec[allmarkerlist$X2])#distance between markers in seconds
intervals=intervals[1:(nrow(allmarkerlist)-1)] #ignore last
#First and last values will be arbitrarily large; others should be around 40 s but may be longer if
#recording interrupted
#Shorter intervals indicate there have been spurious markers

spuriousmarkers=which(intervals<14) #in fact the short ones are the ones we want!
#brief interval  is duration of video
# retain markers with short interval
if (length(spuriousmarkers)>0){
  spuriousmarkers<-c(spuriousmarkers,nrow(allmarkerlist))
  markerlist<-allmarkerlist$X2[spuriousmarkers]#keep first and those with index of spurious marker
  chanlist<-allmarkerlist$X1[spuriousmarkers]
  }
if (length(spuriousmarkers)==0){
  markerlist=allmarkerlist$X2
  chanlist<-allmarkerlist$X2
}

markerlist<-markerlist[!is.na(markerlist)]
chanlist<-chanlist[!is.na(chanlist)]
if(length(markerlist)>30){
  markerlist<-markerlist[2:length(markerlist)]
  chanlist<-chanlist[2:length(chanlist)]
}
thischan<-chanlist[1] #find which marker channel we have selected
for (i in 1:length(chanlist)){
  if (chanlist[i]!=thischan) 
      {thischan<-9}
} #any discrepancy in channel of selected markers will be flagged up as 9
filelist[mysub,startcol+7]<-thischan+6 #marker recorded as channel 7 or 8
#----------------------------------------------------------------------
# Identify excluded trials from xls file
#----------------------------------------------------------------------
myinclude=rep(1,maxtrials) #can default to include all if no data on trials in the excel file
if (ncol(filelist)>3){
  
  myinclude=filelist[mysub,4:(3+maxtrials)] 
}
myremove=which(myinclude==9)#9 indicates trial not given
if (length(myremove)>0)
{ markerlist=markerlist[-myremove]
}
nmarkers=length(markerlist)




#------------------------------------------------------------------
# Settings for initial screening to remove signal dropout
#------------------------------------------------------------------
interpolatebad=1;#set to 1 to replace brief dropout/spiking with mean value for that channel
#number specified here is max number of bad datapoints corrected for
zmultdown=2.5;# lower cutoff for down, because floor for dropoff
#These values may need adjusting individually to ensure good retained and bad lost
zmultup=3.26;
#these zvalues identify channels where dropout or spiking because values
#well outside normal range
#-----------------------------------------------------------
# identify extreme values; can also check each epoch visually
#------------------------------------------------------------------
droprej=rep(0,2) ;spikerej=droprej
zmean=rep(0,2)
mymax=max(rawdata[,2:3])
intmax=100*(1+round(mymax/100))

#this specificies how extreme the signal has to be to count as dropoff or spike
#NB 14/5/17: noted that these had been prespecified at v extreme values - now use zmultdown
# and zmultup
droprej[1]=quantile(L,(1-pnorm(zmultdown)))
droprej[2]=quantile(R,(1-pnorm(zmultdown)))
spikerej[1]=quantile(L,pnorm(zmultup))
spikerej[2]=quantile(R,pnorm(zmultup))

for (i in 1:2){
  if (droprej[i]<1) {droprej[i]=1}#value cannot be 0 or less! Lowest droprej value is 1
}#droprej gives lower limit of signal for L and R channels below which rejected

#-----------------------------------------------------------
# epoch the accepted trials into an array
# This has 4 dimensions; trials,points, L/R, raw/artrej/heartcorr/baselined
myepoched <- array(0, dim=c(nmarkers,postpoints-prepoints+1,2,4)) #initialising array
#------------------------------------------------------------------

mybit=matrix(data = NA, nrow = poiendpoints-prepoints, ncol = 2)

for (mym in 1:nmarkers){
  index1=markerlist[mym]+prepoints
  index2=markerlist[mym]+postpoints
  myepoched[mym,,1,1]=rawdata[index1:index2,2] #L side
  myepoched[mym,,2,1]=rawdata[index1:index2,3] #R side
  #use only data in baseline up to end POI range 

  
  for (i in 1:2){ #same procedure for L then R
    rejpoints<-numeric(0)
    mybit[,i]=myepoched[mym,1:(poiendpoints-prepoints),i,1]
    thisbit=mybit[,i]
    rejpoints=c(rejpoints,which(thisbit < droprej[i])) ;
    rejpoints=c(rejpoints, which(thisbit>spikerej[i]));
    rejpoints=c(rejpoints,which(is.na(thisbit))) #triggered if epoch too short
    #gives list of points where signal indicates dropout or spiking
    if (length(rejpoints)>interpolatebad) {
      myinclude[mym]=-1; #flag with -1; denotes drop this epoch; triggered by either channel
    }
    if (length(rejpoints)==interpolatebad) {
      dropoint=rejpoints;#if just one value abnormal, identify it as dropoint
      myepoched[mym,dropoint,i,1]=zmean[i];  #and substitute the mean for this channel
    }
  }
  timeline=sec[1:(poiendpoints-prepoints)] #time interval in seconds treating start of baseline as zero
  if (initialdatacheck==1) #if initialdatacheck set to zero you skip plotting each trial
  {
    
    #first plot the old values with no correction
    
    plot(timeline+premarker,mybit[,1],type="n"); #add premarker to timeline to get true timings
    lines(timeline+premarker,mybit[,1],col="red")
    lines(timeline+premarker,mybit[,2],col="blue")
    
    #then overplot the corrected values in different colours
    lines(timeline+premarker,myepoched[mym,1:(poiendpoints-prepoints),1,1],col='pink')
    lines(timeline+premarker,myepoched[mym,1:(poiendpoints-prepoints),2,1],col='lightblue')
    abline(v=4) #vertical lines show POI
    abline(v=14)
    
    mytitle=paste(mysubname, 'Trial:', mym,'Include = ',myinclude[mym]+1);
    title(mytitle);
    text(0,80,'Red/blue values in POI have been overwritten with mean',cex=.7)
    text(0,90,'2 = included; 1 = pre-excluded, 0 = rejected',cex=.7);
    cat ("Press [enter] to continue")
    line <- readline()
    dev.off #close figure here?
    
  } #end of if statement
  
  
} #next epoch
detach(rawdata) #rawdata not used beyond this point
#------------------------------------------------------------------
# Remove deleted epochs (originals in origdata; myepoched updated so only has retained epochs)
#------------------------------------------------------------------

keepmarkers=which(myinclude>0)


origdata=myepoched #keep this so can reconstruct
myepoched=myepoched[keepmarkers,,,] #file with only accepted epochs
nmarkers2=length(keepmarkers)
#------------------------------------------------------------------
# Normalise to mean of 100 (see Deppe et al, 2004)
# Multiply by 100 and divide by overall mean value
# ensures results are independent of angle of insonation
#------------------------------------------------------------------
meanL=mean(myepoched[,,1,1])
meanR=mean(myepoched[,,2,1])
myepoched[,,1,1]=100*myepoched[,,1,1]/meanL
myepoched[,,2,1]=100*myepoched[,,2,1]/meanR
#NB. don't use zscore: need to retain variance difference
# so make mean 100, but preserve variance
if (initialdatacheck1==1){
  for (mym in 1:nmarkers2
  ){
    plot(timeline+premarker,myepoched[mym,1:650,1,1],type="n")
    lines(timeline+premarker,myepoched[mym,1:(poiendpoints-prepoints),1,1],col='pink')
    lines(timeline+premarker,myepoched[mym,1:(poiendpoints-prepoints),2,1],col='lightblue')
    cat ("Press [enter] to continue")
    line <- readline()
    dev.off #close figure
  }
}

#-----------------------------------------------------------------
#find heart beat markers and put corrected values in col 2 of 4th dimension
#-----------------------------------------------------------------
#Find peaks with moving window, looking for segments that peak
mypts=dim(myepoched)[2]

for (mym in 1:nmarkers2){
  #print(paste("iteration",mym))
  peaklist=numeric(0)
  thisbit=myepoched[mym,,1,1]
  for (i in seq(3,mypts-3,2)){
    
    if((thisbit[i]>thisbit[i-2])&&(thisbit[i]>thisbit[i+2])&&(thisbit[i-1]>thisbit[i-2])&&(thisbit[i+1]>thisbit[i+3]))
    {peaklist=c(peaklist,i)
    }
    
  }
  peaklist=c(1,peaklist,mypts) #top and tail the list with end values
  peakn=length(peaklist)
  for (p in 1:(peakn-1)){
    myrange=seq(peaklist[p],peaklist[p+1])
    thisheart1=mean(myepoched[mym,myrange,1,1])
    thisheart2=mean(myepoched[mym,myrange,2,1])
    myepoched[mym,myrange,1,2]=thisheart1
    myepoched[mym,myrange,2,2]=thisheart2
  }
  
}
#------------------------------------------------------------------------------
if (initialdatacheck2==1){
  for (mym in 1:nmarkers2 ){
    plot(timeline+premarker,myepoched[mym,1:650,1,1],type="n")
    lines(timeline+premarker,myepoched[mym,1:(poiendpoints-prepoints),1,2],col='pink')
    lines(timeline+premarker,myepoched[mym,1:(poiendpoints-prepoints),2,2],col='lightblue')
    cat ("Press [enter] to continue")
    line <- readline()
    dev.off #close figure
  }
}
#------------------------------------------------------------------------------

# find mean for baseline and subtract this
# this amounts to baseline correction...
#------------------------------------------------------------

nepochbase=nmarkers2
mybase=seq(1,-premarker*samplingrate)

if (baselinecorrect==1){
  for (mym in 1:nmarkers2){
    basemeanL=mean(myepoched[mym,mybase,1,2]) #last dim is 2, which is HB corrected
    basemeanR=mean(myepoched[mym,mybase,2,2])
    myepoched[mym,,1,3]=100+myepoched[mym,,1,2]-basemeanL #last dim 3 is HB and baseline
    myepoched[mym,,2,3]=100+myepoched[mym,,2,2]-basemeanR
  }
} 
#------------------------------------------------------------
# plot after HB correction and baseline correction
#------------------------------------------------------------

if (initialdatacheck3==1){
  for (mym in 1:nmarkers2 ){
    plot(timeline+premarker,myepoched[mym,1:650,1,1],type="n")
    lines(timeline+premarker,myepoched[mym,1:(poiendpoints-prepoints),1,3],col='red')
    lines(timeline+premarker,myepoched[mym,1:(poiendpoints-prepoints),2,3],col='blue')
    mytitle=paste("Epoch",keepmarkers[mym],sep=" ")
    title(mytitle)
    text(-5,110,'blue=R\n red=L\n',cex=.75)
    cat ("Press [enter] to continue")
    line <- readline()
    dev.off #close figure
  }
}
#------------------------------------------------------------
# find and exclude epochs with extreme values in baseline to POI
#------------------------------------------------------------
keepepoch=rep(1,nmarkers2) #initialise for inclusions
for (mym in 1:nmarkers2){
  extremerange=c(which(myepoched[mym,1:(poiendpoints-prepoints),1:2,3]>extremehi),which(myepoched[mym,1:(poiendpoints-prepoints),1:2,3]<extremelo))
  if (length(extremerange)>0 ){
    keepepoch[mym]=0
  }
}
finalepochs=which(keepepoch==1)

#------------------------------------------------------------
# Get grand average and summary stats
#------------------------------------------------------------
finalset=myepoched[finalepochs,,1:2,3]

Lmean <- apply(finalset[,,1], c(2), mean)
Rmean <- apply(finalset[,,2],c(2),mean)
LRdiff=Lmean-Rmean

#Compute LI etc
baseoffset=-premarker*samplingrate
rangestart=baseoffset+poistartpoints
rangeend=baseoffset+poiendpoints
mymax=max(LRdiff[rangestart:rangeend])
mymin=min(LRdiff[rangestart:rangeend])
myside=1;mylatpeak=mymax
if (-mymin>mymax){
  myside=-1 #R biased LI
  mylatpeak=mymin
} #R peak > L peak
mytimepeak=first(which(LRdiff==mylatpeak))
mylatency=(mytimepeak-baseoffset)/samplingrate #need to subtract points for baseline
mypeakrange=seq(mytimepeak-25,mytimepeak+25) #actual points ie includes baseline
myLI=mean(LRdiff[mypeakrange])
indLI=numeric(0)#initialise null vector
myN=length(finalepochs)
for (m in 1:myN){
  indLI=c(indLI,mean(finalset[m,mypeakrange,1]-finalset[m,mypeakrange,2]))
}
mysd=sd(indLI)
myse=mysd/sqrt(myN)
lowCI=myLI-myside*myse*1.96
hiCI=myLI+myside*myse*1.96
lateralised=myside
if((myside*lowCI)<0) {lateralised=0}
latdir=c("R","bilat","L")
mylatdir=latdir[lateralised+2]

if (initialdatacheck4==1){
  #   plot average result
  timelinelong=sec[1:(postmarker*25-prepoints+1)]+premarker
  plot(timelinelong,Lmean, type="n") #set up plot - doesn't actually plot anything
  
  #  plot(timelinelong,LRdiff,type="n")
  lines(timelinelong,Lmean,col='red')
  lines(timelinelong,Rmean,col='blue')
  lines(timelinelong,(100+LRdiff),col='black')
  text(-5,110,'blue=R\n red=L\n black=(L-R) +100',cex=.75)
  title(mysubname)
  cat ("Press [enter] to continue")
  line <- readline()
}



filelist[mysub,34:40]=c(myN,myLI,mylatency,myse,lowCI,hiCI,lateralised)
write.xlsx(filelist, "myfilelist_R2.xlsx",row.names=FALSE)

sprintf('my lateralised is %d', lateralised)
