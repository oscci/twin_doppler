#Questions: are we including sex and age in model, or just lang group and twin pair membership?
# residuals are not normally distributed: problem?

require(yarrr)
require(lme4)
require(tidyverse)

dir<-"C:\\Users\\wilsona\\Dropbox\\project twin kids\\"
file<-"TwinsData_DATA_2017-08-19_1139.csv"
data<-read.csv(paste0(dir,file))

#----------------------------------------------------------
# Produce data frame of included subjects
#----------------------------------------------------------

Nexcluded<-rep(0,3)#records numbers excluded at each step: 1) exclusions based on diagnosis and hearing, etc
#2) by lack of useable fTCD data, 3) by extreme LI, defined as +/-10
data<-filter(data,include>0)
Nexcluded[1]<-388-dim(data)[[1]]
data<-select(data,record_id,fam_id,age_at_test,sex,twin,n_trials,laterality_index,qhp_freq_r,ehp_right,lang_probs,include)
data.short<-filter(data,n_trials> 11)
Nexcluded[2]<-dim(data)[[1]]-dim(data.short)[[1]]
Nexcluded[3]<-length(which(data.short$laterality_index> 10|data.short$laterality_index< -10))
data.short<-filter(data.short,abs(laterality_index)< 10)

#-------------------------------------------------------
# Produce pirate plots and execute multilevel models for 
# the threee laterality measures
#-------------------------------------------------------

laterality_measures<-data.short[,c(7,8,9)]
title<-c("Doppler LI","Quantified hand preference","Edinburgh handedness inventory")
names(data.short)[names(data.short)=="lang_probs"]<-"DLD"

data.short$fam_id<-factor(data.short$fam_id)
data.short$DLD<-factor(data.short$DLD)
data.short$sex<-factor(data.short$sex)

for(i in 1:3){
  pirateplot(data=data.short,laterality_measures[,i]~DLD+sex,ylab=title[i],xlab="1=DLD")
  text<-paste0("Press <ENTER> to see multilevel model summary for ",title[i]," with lang group (DLD or not) as a fixed factor, 
         \nand twin pair as a random factor.")
  readline(text)
  #run mixed effects model, with group (DLD or not) as a fixed factor, and twin pair membership
  #as random factor. Test effect of group. NOTE: issues with normality?
  model<-lmer(laterality_measures[,i]~DLD +(1|fam_id),data=data.short,REML=FALSE)
  print(summary(model))
  readline("Press <ENTER> to see q-q plot.")
  qqnorm(resid(model))
  readline("Press <ENTER> for anova results.")
  print(anova(model))
}
