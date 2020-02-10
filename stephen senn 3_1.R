##stephen senn examplle 3.1

n1<-7 #number of patients first sequence
n2<-6 #number of patients second sequence
n<-n1+n2
seqn<-factor((c(rep(1,n1),rep(2,n2),rep(1,n1),
                rep(2,n2))),labels=c("forsal","salfor")) #sequences
patient<-factor(rep(c("1","4","6","7","0","11",
                      "14","2","3","5","9","12","13"),2)) 
period<-factor(c(rep("1",n),rep("2",n)))
treat<-factor((c(rep(2,n1),rep(1,n2),rep(1,n1),
                 rep(2,n2))), labels=c("salbutamol","formoterol"))
#Note: "formoterol" is coded second level of factor
pef<-c(310, 310,370,410,250,380,330,370,310,380,290,260,
       90,270,260,300,390,210,350,365,385,400,410,320,340,220)
base<-c(290,300,250,390,250,365,190,350,350,350,280,270,
        220,270,270,210,390,240,380,260,345,370,360,290,310,220)

all   <- data.frame(seqn, patient, treat, pef , base)

f<-(nlme::lme(pef~treat * period, random=~1 | patient, data=all, na.action="na.omit" ))
summary(f)$tTable
intervals(f)
f1<-(nlme::lme(pef~treat + period, random=~1 | patient, data=all, na.action="na.omit" ))
summary(f1)$tTable
f1<-update(f1, method="ML")
f<-update(f, method="ML")
anova(f,f1)
