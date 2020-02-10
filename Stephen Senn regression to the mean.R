

#regression to the mean
#http://eprints.gla.ac.uk/8107/1/id8107.pdf
n=1000
r=0.79
sdt=8
mu=90

x= rnorm(n)*sdt
z= rnorm(n)*sdt
y= x*r+z*(1-r^2)^.5

y<- (y)+mu
x<- (x)+mu

cor(x,y)
plot(y~x,   col=ifelse(y>95 & x>95, 'red',
                       ifelse(y>95 & x<95, 'orange',
                              ifelse(y<95 & x>95, 'orange',
                                     ifelse(y<95 & x<95, 'blue',
                                     'black'
                              )))), xlim=c(60,120), ylim=c(60,120),
     xlab="baseline", ylab="at outcome")
abline(h=95)
abline(v=95)

t.test(y, x, paired=T)

##now select extreme 
y1<- y[x>95]
x1<- x[x>95] 
plot(y1~x1, main="outcome less than baseline",
     col=ifelse(y1>95 & x1>95, 'red',
                       ifelse(y1>95 & x1<95, 'orange',
                              ifelse(y1<95 & x1>95, 'orange',
                                     ifelse(y1<95 & x1<95, 'blue',
                                            'black'
                                     )))), xlim=c(60,120), ylim=c(60,120),
     xlab="baseline", ylab="at outcome")
abline(h=95)
abline(v=95)

t.test(y1, x1, paired=T)


##select high responders
##outcome on ave higher than baseline
y1<- y[y>95]
x1<- x[y>95] 
plot(y1~x1, main="outcome on average > than baseline",
     col=ifelse(y1>95 & x1>95, 'red',
                ifelse(y1>95 & x1<95, 'orange',
                       ifelse(y1<95 & x1>95, 'orange',
                              ifelse(y1<95 & x1<95, 'blue','black'
                              )))), xlim=c(60,120), ylim=c(60,120),
                                xlab="baseline", ylab="at outcome")
abline(h=95)
abline(v=95)

t.test(y1, x1, paired=T)

##responders

n=1000
r=0 
sdt=5
mu=-10

x= rnorm(n)*sdt
z= rnorm(n)*sdt
y= x*r+z*(1-r^2)^.5

y<- (y)+mu
x<- (x)+mu

cor(x,y)
resp<--5
plot(y~x,   col=ifelse(y>resp & x>resp, 'red',
                       ifelse(y>resp & x<resp, 'orange',
                              ifelse(y<resp & x>resp, 'orange',
                                     ifelse(y<resp & x<resp, 'blue',
                                            'black'
                                     )))), xlim=c(-30,10), ylim=c(-30,10),
     xlab="baseline", ylab="at outcome")
abline(h=-10);abline(v=-10)
abline(h=-5);abline(v=-5)
   
 
length(y[x<resp])/ length(y)
left<-x<resp
summary(left)

outcome <-ifelse(y>resp,1,0)
baseline<-ifelse(x>resp,1,0)
tab<-table(outcome, baseline)
margin_table <- addmargins(tab)

tb <-  table(outcome, baseline,
                       dnn=c("outcome", "baseline") ) 
colnames(tb)<-rownames(tb) <- c("resp","non resp")   # rename rows
dd <- addmargins(tb, FUN = list(Total = sum), quiet = TRUE)

# prop.table(tab)
r1<-(dd[1,1]+dd[1,2])
paste("Of the ",r1,"patients who responded in the first cross-over"
,dd[1,1]," responded in the second. The proportion, therefore is",
dd[1,1]/(dd[1,1]+dd[1,2]))

 
r1<-(dd[2,1]+dd[2,2])
paste("Of the ",r1,"patients who did not respond in the first cross-over"
      ,dd[2,1]," responded in the second. The proportion, therefore is",
      dd[2,1]/(dd[2,1]+dd[2,2]))




 