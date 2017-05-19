
setwd("~/Dropbox/R/DB_otherscripts/Doppler/Doppler2")

fileName2='NLA1550.TW0'



f <- file(fileName2, open="rb",blocking=TRUE)   # UTF-16LE, which is "called" Unicode in Windows

in_data=readBin(f,what='integer',n=900000,size=2)
blk = 64; # number of single channel samples in a row
ch = 6; # number of channels
cycle = blk*ch
Ncycle = length(in_data)/cycle 

for (j in 1:Ncycle){
  j1=blk*6*(j-1)+1
  j2=j*blk*6
  mychunk<-matrix(in_data[j1:j2],ncol=6)
  if(j==1){bigdata<-mychunk}
  if(j>1){bigdata<-rbind(bigdata,mychunk)}
}


x=20000:24000
y<-bigdata[x,1]
z<-bigdata[x,2]
q<-bigdata[x,3]
r<-bigdata[x,4]
q1<-bigdata[x,5]
r1<-bigdata[x,6]
plot(x,y, type="n") #set up plot - doesn't actually plot anything
lines(x,y,col="red")
lines(x,z,col="lightblue")
lines(x,q,col='black')
#lines(x,r,col='darkgreen')
lines(x,q1,col='purple')
lines(x,r1,col='brown')

#observations: size of marker v different from file to file
#channel 6 seems to be the relevant one
# sometimes marker > 5 is criterion and then separated by 12 sec
# sometimes v big marker, just once per trial
