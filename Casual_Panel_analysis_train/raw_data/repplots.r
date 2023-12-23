# replication code
# set working directory

rm(list=ls())
install.packages("beanplot","RColorBrewer","foreign","ggplot2","lattice","Matching","lmtest","sandwich","plotrix","xtable")
library(beanplot)
library(RColorBrewer)
library(foreign)
library(ggplot2)
library(lattice)
library(Matching)
library(lmtest)
library(sandwich)
library(plotrix)
library(xtable)

##################################################
## Figure 1: Trends in Naturalization Institutions
d <- read.dta("fig1.dta",convert.factors = F)
mypal <-  "black"
var1 <- c("dd_institution_linearB3","dd_institution_linearB2","dd_institution_linearB1")

try(dev.off())
pdf("InstPlot_All_1990_2010_bw.pdf",width=9,height=7)
matplot(d[,var1]*100,
        x=d$year,type="l",ylim=c(0,85),lty=1:3,lwd=3,xlab="Year",yaxs="i",ylab="Precent of Municipalities",col=mypal,xaxs="i")
grid(nx = 20, ny = 5, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend("left",legend=c("Direct Democracy","Representative Democracy","Appointed Commission"),
lty=1:3,col=mypal,lwd=2,cex=1.2,bg="white")
dev.off()


##############################################################
## Figure 2: Effect of Direct Democracy on Naturalization Rates
rm(list=ls())
d <- read.dta("fig2.dta")
summary(d)
length(table(d$bfs))

X=d$year_zero[abs(d$year_zero)<= 8]
Y=d$nat_rate_ord[abs(d$year_zero)<= 8]

try(dev.off())

pdf(file="lottkenny_bw.pdf",width=9,height=7)
plot(rnorm(1000),
     pch="+",
     col="black",
     cex=.7,
     ylim=c(0,6),
     xlim=c(-7,7),
     type="n",
     xlab="Years Before / After Change from Direct to Representative Democracy",
     ylab = "Naturalization Rate (%)"      
)
points(jitter(X, factor=.5),Y,col=gray(.85),pch=".",cex=1)      
abline(v=0,col="black",lty="dotted", lwd=2)

lout1 <- loess(Y[X<0]~X[X<0],span=1)
lout2 <- loess(Y[X>=0]~X[X>=0],span=1)
lines(approx(X[is.na(X)==F & is.na(Y)==F & X<0],lout1$fitted),
      col="black",lwd=3)
lines(approx(X[is.na(X)==F & is.na(Y)==F & X<0],lout1$fitted + 1.96*predict(lout1,se=T)$se.fit),
      col="black",lwd=2,lty="dotdash")
lines(approx(X[is.na(X)==F & is.na(Y)==F & X<0],lout1$fitted - 1.96*predict(lout1,se=T)$se.fit),
      col="black",lwd=2,lty="dotdash")

lines(approx(X[is.na(X)==F & is.na(Y)==F & X>=0],lout2$fitted),
      col=gray(.45),lwd=3, lty="longdash")
lines(approx(X[is.na(X)==F & is.na(Y)==F & X>=0],lout2$fitted + 1.96*predict(lout2,se=T)$se.fit),
      col=gray(.45),lwd=2,lty="dotdash")
lines(approx(X[is.na(X)==F & is.na(Y)==F & X>=0],lout2$fitted - 1.96*predict(lout2,se=T)$se.fit),
      col=gray(.45),lwd=2,lty="dotdash")

legend("bottomleft",c("Direct Democracy"),
       lty=c("solid"), lwd=rep(2),
       col=c("black"), inset=c(.12,.05),
       bg="white", cex=1)

legend("bottomright",c("Representative Democracy"),
       lty=c("longdash"), lwd=rep(2),
       col=c(gray(.45)), inset=c(.07,.05),
       bg="white", cex=1)
dev.off()

#######################################################################
## Figure 3: Dynamic Effect of Direct Democracy on Naturalization Rates
rm(list=ls())
d<-read.table("fig3.txt",header=TRUE)
min = -5
max = 3

bblue  <- "black"
rred   <- gray(.45)

try(dev.off())
pdf("autor_plot19bw.pdf",width=10,height=7.5)
plot(rnorm(1000),
     pch="+",
     col="black",
     cex=.7,
     ylim=c(-65,189),
     xlim=c(min,max),
     type="n",
     xaxt="n",
     xlab = "Years Before / After Change from Direct to Representative Democracy",      
     ylab = "Percent Change in Naturalization Rate",
     cex.axis=1.3,cex.lab=1.3
)
staxlab(1,at=min:max,nlines=2, top.line=1, line.spacing=0,cex=1.3)

meanDD=1.8585
X <- c(min:max)
Y <- d[(max-min+1):1,1]/meanDD*100
SE <- d[(max-min+1):1,2]/meanDD*100

abline(h=0,col="black",lwd=2, lty=2)

for (t in min:-1) {
  segments(t, (Y[t+max+3]-1.96*SE[t+max+3]), t, (Y[t+max+3]+1.96*SE[t+max+3]),col=bblue,lwd=2)
  points(X[t+max+3],Y[t+max+3],col=bblue,pch=19,cex=1.4)
}
for (t in 0:max) {
  segments(t, (Y[t+max+3]-1.96*SE[t+max+3]), t, (Y[t+max+3]+1.96*SE[t+max+3]),col=rred,lwd=2)
  points(X[t+max+3],Y[t+max+3],col=rred,pch=19,cex=1.4)
  
}
dev.off()

####################################################################################################
## Figure 4: Dynamic Effect of Direct Democracy on Facilitated Naturalization Rates (Placebo Outcome)
d<-read.table("fig4.txt",header=TRUE)

try(dev.off())
pdf("autor_plotPlacebo_bw.pdf",width=10,height=7.5)
plot(rnorm(1000),
     pch="+",
     col="black",
     cex=.7,
     ylim=c(-65,189),
     xlim=c(min,max),
     type="n",
     xaxt="n",
     xlab = "Years Before / After Change from Direct to Representative Democracy",      
     ylab = "Percent Change in Facilitated Naturalization Rate",
     cex.axis=1.3,cex.lab=1.3
)
staxlab(1,at=min:max,nlines=2, top.line=1, line.spacing=0,cex=1.3)

meanDD=2.14

X <- c(min:max)
Y <- d[(max-min+1):1,1]/meanDD*100
SE <- d[(max-min+1):1,2]/meanDD*100

abline(h=0,col="black",lwd=2, lty=2)

for (t in min:-1) {
  segments(t, (Y[t+max+3]-1.96*SE[t+max+3]), t, (Y[t+max+3]+1.96*SE[t+max+3]),col=bblue,lwd=2)
  points(X[t+max+3],Y[t+max+3],col=bblue,pch=19,cex=1.4)
  
}

for (t in 0:max) {
  segments(t, (Y[t+max+3]-1.96*SE[t+max+3]), t, (Y[t+max+3]+1.96*SE[t+max+3]),col=rred,lwd=2)
  points(X[t+max+3],Y[t+max+3],col=rred,pch=19,cex=1.4)
  
}

dev.off()


#############################################################
## Figure 5: Effect of Direct Democracy on Naturalization Rates by Country of Origin
rm(list=ls())
# all
d <- read.table("fig5a.txt",header=T,check.names=F)
names(d) <- c("pe","se","lb","ub")
d$outcome <- rownames(d)
d$region <- "All"
d1 <- d
# german
d <- read.table("fig5b.txt",header=T,check.names=F)
names(d) <- c("pe","se","lb","ub")
d$outcome <- rownames(d)
d$region <- "German"

d <- rbind(d1,d)

d$gruppe <- factor(d$outcome,levels=c("nat_rate_ord_yutu","nat_rate_ord_southeu","nat_rate_ord_richeu","nat_rate_ord"),
                   labels=c("All Countries","Western/Northern Europe","Southern Europe","Yugoslavia/Turkey")[4:1])


# Test for significance of diff
# all muni
(diff.yutu.richeu = (d$pe[2] - d$pe[3])/sqrt(d$se[2]^2 + d$se[3]^2))
(diff.yutu.southeu = (d$pe[2] - d$pe[4])/sqrt(d$se[2]^2 + d$se[4]^2))
# German muni
(diff.yutu.richeu = (d$pe[6] - d$pe[7])/sqrt(d$se[6]^2 + d$se[7]^2))
(diff.yutu.southeu = (d$pe[6] - d$pe[8])/sqrt(d$se[6]^2 + d$se[8]^2))

d$Region <- factor(d$region,levels=c("All","German"))
f5 = ggplot(data = d, aes(x = gruppe, y = pe, width=0.5,ymin = lb, ymax = ub,
                          fill=Region,shape=Region))  
f5 = f5 + geom_hline(yintercept = 0,size=1,colour="black",linetype="dotted")
f5 = f5 + geom_pointrange(width=0.5,size=1,position = position_dodge(width=-.5))
f5 = f5 + scale_y_continuous(name="Percent Change in Naturalization Rate") 
f5 = f5 + scale_x_discrete(name=" ")
f5 = f5 + scale_shape_manual(values=c(21,22))  # as before
f5 = f5 + scale_fill_manual(values=c("black","white"))
f5 = f5 + coord_flip()
print(f5)

theme_bw1 <- function(base_size = 12, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) #% +replace%
  theme(
    axis.text         = element_text(size = base_size , colour = "black"),
    axis.ticks        = element_line(colour = "grey50")
  )
}

f5 = f5  + theme_bw1()  
print(f5)

try(dev.off())
pdf("figcoo_bw1.pdf",width=10,height=6.5)
f5 = f5  + theme_bw1()
print(f5)
dev.off()



#############################################################
## Figure 6: Effect of Direct Democracy and Voter Preferences
rm(list=ls())
d  <- read.table("fig6.txt",header=T)
dd <- na.omit(read.dta("svpconstzero.dta"))

meanDD=2
d$pe <- d$c1/meanDD*(-100)
d$se <- d$c2/meanDD*(-100)

d$lb <- d$pe- 1.96*d$se
d$ub <- d$pe+ 1.96*d$se
d$svp <- d$c3
d <- d[d$svp<53,]
d <- d[-1,]
ymax <- 135
ymin <- -35
yylim <- c(ymin,ymax)

try(dev.off())
pdf("figinteract_lin2.pdf",width=10,height=7.5)
plot(y=d$pe,d$svp,type="l",lwd=2,col="black",
     ylab="Percent Change in Naturalization Rate",
     xlab="Municipality Level SVP Vote Share (%)",
     #ylim = c(-2,.5),xlim=c(0,64),
     ylim = yylim,xlim=c(0,52),
     xaxs="i",yaxs="i",yaxt="n",
     cex.axis=1.3,cex.lab=1.3
)


axis(side=2,at=seq(0,ymax,20),cex.axis=1.3,cex.lab=1.3)

lines(y=d$lb,d$svp,lwd=2,lty=2,col="black")
lines(y=d$ub,d$svp,lwd=2,lty=2,col="black")
abline(h=0,lty=3)


beanplot(dd$svpconstzero,horizontal=T,
         add=TRUE,at=min(yylim),what=c(0,1,1,0),cutmin=0,
         beanlines="quantiles",side="second",boxwex =50,
         col=c(grey(.7),"red","red","red"),
         overallline = "median",log="y",beanlinewd=1)

abline(v=0,lwd=1)
legend("topleft",legend=c("Effect: Direct to Representative Democracy",
                          "95 % Confidence Interval"),
       lty=c(1,2),col=c("black","black"),lwd=2,cex=1.2)

legend(35.5,min(yylim)+20,legend=c("Quartiles SVP Vote Share")
       ,pch=c("|"),col=c("red"),cex=1.2,bg="white")
abline(h=-10,lwd=1)
abline(v=53,lwd=1)
abline(h=min(yylim)+.15,lty=1)
dev.off()



#####################################################################################
## Figure B.1: Effect of Direct Democracy and SVP Vote Shares (Nonlinear Interaction)
rm(list=ls())
d <- read.table("figb1.txt",header=T)
d <- d[-1,]
meanDD=2
d$pe <- d$c1/meanDD*(-100)
d$se <- d$c2/meanDD*(-100)

d$lb <- d$pe- 1.96*d$se
d$ub <- d$pe+ 1.96*d$se
d$svp <- d$c3
d$svp <- d$c3 + .5

d$gruppe <- factor(d$svp,labels=c("Low","Medium","High"))
d <- d[order(d$gruppe),]
p = ggplot(d,aes(y=pe,x=gruppe,colour = "black"))
p = p +  geom_hline(yintercept = 0,size=1,colour="black",linetype="dotted")
p = p +geom_pointrange(aes(ymin=lb,ymax=ub,width=.5),position="dodge",size=1,colour="black")
p = p + scale_y_continuous(name="Percent Change in Naturalization Rate") 
p = p + scale_x_discrete(name="Municipality Level SVP Vote Share (%)") 
print(p)

theme_bw1 <- function(base_size = 14, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) #% +replace%
  theme(
    axis.text         = element_text(size = base_size , colour = "black"),
    axis.ticks        = element_line(colour = "grey50"),
    axis.title =  element_text(size = base_size , colour = "black"),
    legend.position = "none"
  )
}


dev.off()
pdf("figinteract_nonlin1.pdf",width=10,height=7.5)
p = p  + theme_bw1()
print(p)
dev.off()


#############################################################
## Figure B.2: Effect of Direct Democracy and SVP Seat Shares
rm(list=ls())
d <- read.table("figb2.txt",header=T)
d <- d[-1,]
meanDD= 2.02 
d$pe <- d$c1/meanDD*(-100)
d$se <- d$c2/meanDD*(-100)

d$lb <- d$pe- 1.96*d$se
d$ub <- d$pe+ 1.96*d$se
d$svp <- d$c3
d$svp <- d$c3 + .5


d$gruppe <- factor(d$svp,labels=c("Low: Mean = 0","Medium: Mean = 20","High: Mean = 49"))

d <- d[order(d$gruppe),]
p = ggplot(d,aes(y=pe,x=gruppe,colour = "black"))
p = p +  geom_hline(yintercept = 0,size=1,colour="black",linetype="dotted")
p = p +geom_pointrange(aes(ymin=lb,ymax=ub,width=.5),position="dodge",size=1,colour="black")
p = p + scale_y_continuous(name="Percent Change in Naturalization Rate") 
p = p + scale_x_discrete(name="Municipality Level SVP Seate Share in Executive Council (%)") 
print(p)

theme_bw1 <- function(base_size = 14, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) #% +replace%
  theme(
    axis.text         = element_text(size = base_size , colour = "black"),
    axis.ticks        = element_line(colour = "grey50"),
    axis.title =  element_text(size = base_size , colour = "black"),
    legend.position = "none"
  )
}

dev.off()
pdf("figinteract_exe2_nonlin.pdf",width=10,height=7.5)
p = p  + theme_bw1() 
print(p)
dev.off()

## Figure B.3: Local SVP Seat Shares and Federal SVP Vote in Municipality
rm(list=ls())
d<-read.dta("representation_aff.dta")
min = 1
max = 6

pdf("representation_aff.pdf",width=10,height=7.5)

plot(rnorm(1000),
     pch="+",
     col="black",
     cex=.7,
     ylim=c(0,40),
     xlim=c(min,max),
     type="n",
     xaxt="n",
     xlab = "SVP Vote Share in Federal Elections for National Legislative Council (in %)",      
     ylab = "Seats in Municipal Executive Council (in %)",
     cex.axis=1.3,cex.lab=1.3
)
voteshares <- c("[0-10]",  "(10-20]",  "(20-30]",  "(30-40]",	"(40-50]",  "(50-99]")
staxlab(1,at=min:max,labels=voteshares,nlines=2, top.line=1, line.spacing=0,cex=1.3)

X <- c(min:max)
Y <- cbind(d$pe0,d$pe1,d$pe2,d$pe3,d$pe4,d$pe5) *100
SE <- cbind(d$se0,d$se1,d$se2,d$se3,d$se4,d$se5) *100

points(X-.1,Y[1,],col="blue",pch=19,cex=1.4)
segments(X-.1, (Y[1,]-1.96*SE[1,]), X-.1, (Y[1,]+1.96*SE[1,]),col="blue",lwd=2)
segments(X[1:5]-.1, Y[1,1:5], X[2:6]-.1, Y[1,2:6],col="blue",lwd=2, lty="dotted")

points(X-.03,Y[2,],col="orange",pch=19,cex=1.4)
segments(X-.03, (Y[2,]-1.96*SE[2,]), X-.03, (Y[2,]+1.96*SE[2,]),col="orange",lwd=2)
segments(X[1:5]-.03, Y[2,1:5], X[2:6]-.03, Y[2,2:6],col="orange",lwd=2, lty="dotted")

points(X+.03,Y[4,],col="red",pch=19,cex=1.4)
segments(X+.03, (Y[4,]-1.96*SE[4,]), X+.03, (Y[4,]+1.96*SE[4,]),col="red",lwd=2)
segments(X[1:5]+.03, Y[4,1:5], X[2:6]+.03, Y[4,2:6],col="red",lwd=2, lty="dotted")

points(X+.1,Y[3,],col="darkgreen",pch=19,cex=1.4)
segments(X+.1, (Y[3,]-1.96*SE[3,]), X+.1, (Y[3,]+1.96*SE[3,]),col="darkgreen",lwd=2)
segments(X[1:5]+.1, Y[3,1:5], X[2:6]+.1, Y[3,2:6],col="darkgreen",lwd=2, lty="dotted")

legend("topright",legend=c("FDP","CVP","SP","SVP"),
       lty=c(1,1,1,1),col=c("blue","orange","red","darkgreen"),lwd=2)

dev.off()

## Table B.1: Naturalization Regimes in Swiss Municipalities (1990-2010) 
d <- read.dta("repdata_public.dta")
d <- na.omit(d[,c("inst","year")])

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

tt <- c()
lab <- c()
start <- 1990
for(i in 1:7){
cat(start,"\n")  
t1 <- table(d$inst[d$year>=start & d$year<=start+2])
t1 <- round(t1/sum(t1)*100,1)
tt <- cbind(tt,t1)
lab <- c(lab,paste(substrRight(start,2)
,"-",substrRight(start+2,2),sep=""
))
start <- start + 3
}
colnames(tt) <- lab
xtable(tt,file="",digits=1)

# Appendix D

# Figures D1-D3 ESS

rm(list=ls())
d1 <- read.csv("codesess.csv",stringsAsFactors = FALSE,header = FALSE)
colnames(d1) <- c("country","code")

datas <- c("imwbcnt","imueclt","imdfetn","imsmetn","impcntr")

mains <- c("'[Country] is made a worse place to live by immigration'",
           "'[Country]'s cultural life is undermined by immigrants'",
           "'Allow none/few immigrants of different race/ethnicity from majority to come'",
           "'Allow none/few immigrants of same race/ethnicity as majority to come'",
           "'Allow none/few immigrants from poorer countries outside Europe to come'")


for(k in 1:5){
  d <- read.table(paste(datas[k],".txt",sep=""))
  colnames(d) <- c("pe","se")
  d$ub <- d$pe + 2*d$se
  d$lb <- d$pe - 2*d$se
  d$code <- rownames(d)
  d
  dim(d)
  d <- merge(d,d1,by="code")
  
  d <- d[order(-1*d$pe),]
  
  d$index <- 1:nrow(d)
  
  pdf(paste("ESS",k,".pdf",sep=""),width=8,height=6)
  plot(d$pe,x=d$index,ylim=c(0,120),xlab="rank",ylab="% of Voters",pch=19,
       main=mains[k],xaxt="n",yaxs="i",yaxt="n")
  axis(side=2,at=seq(0,100,10),labels=seq(0,100,10),cex.axis=.7)
  axis(side=1,at=d$index,labels=d$index,cex.axis=.7)
  arrows(d$index,y0=d$lb,y1=d$ub,code=3,angle=90,length=.1)
  text(x=d$index,y=100,labels=d$country,srt=90,cex=.8)
  CH <- which(d$code=="CH")
  arrows(d$index[CH],y0=d$lb[CH],y1=d$ub[CH],code=3,angle=90,length=.1,pch=19,col="red")
  points(x=d$index[CH],d$pe[CH],col="red",pch=19)
  text(x=d$index[CH],y=100,labels=d$country[CH],srt=90,col="red",cex=.8)
  dev.off() 
}

# Figures D4 ISSP
rm(list=ls())
datas <- c("issp1","issp2") # CIs computed from ISSP Website 

mains <- c("'Strict limits/Prohibit people from other countries to come and work here'",
           "'Employers should give priority to [country] people over immigrants'")


for(k in 1:2){
  
  d <- read.csv(paste(datas[k],".csv",sep=""),stringsAsFactors = FALSE,header = TRUE)
  
  d$p <- d$pe/100
  d$se <- sqrt(
    (( d$p *(1-d$p)) / d$N) *(100^2)
  )
  
  d$ub <- d$pe + 2*d$se
  d$lb <- d$pe - 2*d$se
  d <- d[order(-1*d$pe),]
  d$index <- 1:nrow(d)
  
  pdf(paste("ISSP",k,".pdf",sep=""),width=8,height=6)
  plot(d$pe,x=d$index,ylim=c(0,130),xlab="rank",ylab="% of Voters",pch=19,
       main=mains[k],xaxt="n",yaxs="i",yaxt="n")
  axis(side=2,at=seq(0,100,10),labels=seq(0,100,10),cex.axis=.7)
  axis(side=1,at=d$index,labels=d$index,cex.axis=.7)
  arrows(d$index,y0=d$lb,y1=d$ub,code=3,angle=90,length=.1)
  text(x=d$index,y=110,labels=d$country,srt=90,cex=.7)
  CH <- which(d$country=="Switzerland")
  arrows(d$index[CH],y0=d$lb[CH],y1=d$ub[CH],code=3,angle=90,length=.1,pch=19,col="red")
  points(x=d$index[CH],d$pe[CH],col="red",pch=19)
  text(x=d$index[CH],y=110,labels=d$country[CH],srt=90,col="red",cex=.7)
  dev.off()
  
}

