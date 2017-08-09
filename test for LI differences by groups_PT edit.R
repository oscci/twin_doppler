#
require(yarrr)

dir<-"C:\\Users\\wilsona\\Dropbox\\project twin kids\\"
source(paste0(dir,"wrangle_twindata.R"))

#----------------------------------------------------------
# Check for between group differences in twins with a possible
# genetic vulnerability to language impairment
#----------------------------------------------------------

#bind twin groups for later analysis, and assign group membership;
#1 indicates that one or both twins has a language impairment
alltwins<-rbind(paired_dz_lat,paired_dzo_lat,paired_mz_lat)
lang_group<-rep(0,length(alltwins$record_id))
for(i in 1:length(alltwins$record_id)){
  if(alltwins$lang_probs[i]==1|alltwins$lang_probs.2[i]==1){
    lang_group[i]<-1
  }
}

#alltwins<-cbind(alltwins,group)
#t.test(alltwins$laterality_index~alltwins$group)
#t.test(alltwins$laterality_index.2~alltwins$group)
#chisq.test(alltwins$lateralised_category,alltwins$group)
#chisq.test(alltwins$lateralised_category.2,alltwins$group)

#Need to unpair twins for analysis in mixed effects model
#colnames(alltwins)<-c(colnames(alltwins[1:13]),colnames(alltwins[1:13]))

twin1.dat<-alltwins[,1:14]
twin2.dat<-alltwins[,15:28]

names(twin2.dat)<-colnames(alltwins[,1:14])

alltwins_comb<-rbind(twin1.dat,twin2.dat)
alltwins_comb$lang_group<-c(lang_group,lang_group)
#alltwins<-cbind(alltwins,lang_group)

pirateplot(data=alltwins_comb,laterality_index~lang_group+sex,ylab="LI",xlab="1=Genetic vulnerability to language difficulties")
readline("Press <ENTER> to see multilevel model summary, with lang group, age and sex as fixed factors, \nand twin pair as random factor")

alltwins_comb$fam_id<-factor(alltwins_comb$fam_id)
alltwins_comb$lang_group<-factor(alltwins_comb$lang_group)
alltwins_comb$sex<-factor(alltwins_comb$sex)

require(lme4)

#run mixed effects model, with group, age and sex as fixed factors, and twin pair membership
#as random factor. Test effect of group. NOTE: issues with normality?
model=lmer(laterality_index~lang_group + age_at_test + sex + (1|fam_id),data=alltwins_comb,REML=FALSE)
null=lmer(laterality_index~age_at_test + sex + (1|fam_id),data=alltwins_comb,REML=FALSE)
print(summary(model))
qqnorm(resid(model))
readline("Press <ENTER> to see null model summary, with age and sex as fixed factors, \nand twin pair as random factor")
print(summary(null))

readline("Press <ENTER> for anova results")
print(anova(model,null))

#####

#####

plot(model,type=c("p","smooth"))

plot(model,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(alltwins_comb$lang_group==1,"red","blue"),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid))))

plot(model,resid(.,type="pearson")~age_at_test, type=c("p","smooth"))

qqnorm(resid(model), col=ifelse(alltwins_comb$lang_group==1,"red","blue"))


levels(alltwins_comb$sex)<-c("male","female")
levels(alltwins_comb$lang_group)<-c("TD","DLD")

ggplot(alltwins_comb,aes(x=age_at_test,y=laterality_index,colour=lang_group,type=sex))+
    geom_point(alpha=0.7)+
    geom_smooth(method="lm",alpha=0.3,aes(group=lang_group))+facet_grid(~sex)+theme_bw()
    

ggplot(alltwins_comb,aes(laterality_index,colour=lang_group,fill=lang_group))+
    geom_density(alpha=0.7)+facet_grid(~sex)+theme_bw()
    


