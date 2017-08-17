#Script to create language categories from other variables
# By DVM Bishop 16 08 2017

#Reads in Recap data and writes new column
#Basic processing in tidyverse

############################################################
library(tidyverse)
library(ggplot2)
library(yarrr)
############################################################
#load csv data which is created by export option from Redcap
file.loc<-"~/Dropbox/ERCadvanced/project twin kids/"
filename<-c("TwinsData_DATA_2017-08-17_0704.csv") 
my_dat<-data.frame(read_csv(paste0(file.loc,filename)))

my_dat$lang_probs_old<-my_dat$lang_probs #create backup of lang probs


#Now count up how many lang and reading tests done, and how many more than 1SD below avg
my_dat$Nlowlang<-0 #Initialise counters to zero
my_dat$Nlowread<-0 
my_dat$langdone<-8 #assume all done but then subtract if not
my_dat$readdone<-5

langtests<-c('wasi_vocab_ss','wdck_jhsn_ss','sent_rep_ss','nonword_rep_ss','oromotor_range',
             'phab_pic_ss','phab_digit_ss','gcc')
langcuts<-c(40,85,7,7,3,85,85,55)
readtests<-c('towre_words_ss','towre_nonwords_ss','nara_acc_ss','nara_comp_ss','nara_rate_ss')
readcuts<-c(85,85,85,85,85)

#Count how many language tests done and how many below cutoff
for (i in 1:length(langtests)){
  thiscol<-which(colnames(my_dat)==langtests[i])
  myplus<-which(my_dat[,thiscol]<langcuts[i])
  my_dat$Nlowlang[myplus]<-my_dat$Nlowlang[myplus]+1
  mymiss<-c(which(is.na(my_dat[,thiscol])),which(my_dat[,thiscol]>900))
  my_dat$langdone[mymiss]<-my_dat$langdone[mymiss]-1
}
#Count how many reading tests done and how many below cutoff
for (i in 1:length(readtests)){
  thiscol<-which(colnames(my_dat)==readtests[i])
  myplus<-which(my_dat[,thiscol]<readcuts[i])
  my_dat$Nlowread[myplus]<-my_dat$Nlowread[myplus]+1
  mymiss<-c(which(is.na(my_dat[,thiscol])),which(my_dat[,thiscol]>900))
  my_dat$readdone[mymiss]<-my_dat$readdone[mymiss]-1
}

alllow=my_dat$Nlowread+my_dat$Nlowlang
checkdiff<-my_dat$n_language_low-alllow
# Can look at checkdiff to identify cases inconsistent with prior scoring.
# Initial check suggests that where discrepancies occurred these related to missing data or unscored tests

langcases<-which(alllow>1)

my_dat$lang_probs<-0 #Initialise lang probs to zero
my_dat$lang_probs[langcases]<-1

#find ones where hearing mentioned in notes for exclusion
noteslist<-regexpr('hear',my_dat$notes_on_exclusion)
hearcase<-which(noteslist>0)
my_dat$lang_probs[hearcase]<-4
noteslist<-regexpr('autis',my_dat$notes_on_exclusion)
autcase<-which(noteslist>0)
my_dat$lang_probs[autcase]<-2
noteslist<-regexpr('ASD',my_dat$other_diagnosis)
autcase<-which(noteslist>0)
my_dat$lang_probs[autcase]<-2
noteslist<-regexpr('Asper',my_dat$other_diagnosis)
autcase<-which(noteslist>0)
my_dat$lang_probs[autcase]<-2
iqcase<-which(my_dat$iq<70)
my_dat$lang_probs[iqcase]<-3
hi_srs<-which(my_dat$srs_t_score>75)
my_dat$lang_probs[hi_srs]<-2

#redo inclusion status
myexclude<-which(my_dat$lang_probs>1)
my_dat$include[myexclude]<-0

#check language and reading status
my_dat$dld_rd<-0
my_dat$dld_rd[myexclude]<-99
my_dat$myrow<- seq.int(nrow(my_dat))
LIRD<-filter(my_dat,Nlowlang>1,Nlowread>1,include==1)
LIonly<-filter(my_dat,Nlowlang>1,Nlowread<2,include==1)
RDonly<-filter(my_dat,Nlowlang<2,Nlowread>1,include==1)
my_dat$dld_rd[LIRD$myrow]<-11
my_dat$dld_rd[LIonly$myrow]<-10
my_dat$dld_rd[RDonly$myrow]<-1
table(my_dat$dld_rd)

#new value for n_language_low
my_dat$n_language_low<-alllow
my_dat$n_language_complete<-my_dat$langdone+my_dat$readdone

fornewfile<-select(my_dat,record_id,n_language_low,n_language_complete,lang_probs,include,dld_rd)
write.csv(fornewfile, file = "forimport.csv",row.names=FALSE)
#This file can be used to import the revised/new cols to Redcap

#-----------------------------------------------------------------------------------------------------
#NB when importing to Redcap, inconsistencies are displayed. These need to be checked before final import
#Most seemed to result from incomplete testing or missing data
#This was used for comparison
#From original Scoring Sheet twins (3).xls I created a csv for upload called: import_updated_langtests.csv
#Then did check against what was already on Redcap from "TwinsData_DATA_2017-08-16_1247.csv" 
# by exporting selected variables as per script below
#Confirmed that Redcap version was missing some late data entries that were on xls.

# forcheckfile<-select(my_dat,record_id, towre_words_ss, towre_nonwords_ss, wdck_jhsn_ss, 
#                      nonword_rep_ss, sent_rep_ss, oromotor_range, nara_acc_ss, nara_comp_ss,
#                      nara_rate_ss, phab_pic_ss, phab_digit_ss)
# setwd("~/Dropbox/ERCadvanced/project twin kids")
# write.csv(forcheckfile, file = "forcheckfile.csv",row.names=FALSE)
#-----------------------------------------------------------------------------------------------------
#