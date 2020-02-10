#Senn individual therapy

##odds ratio, reconstructing
pla<-.098*3837
trt<-.072*3837
pla.n<-(1-.098)*3837
trt.n<-(1-.072)*3837
(trt*pla.n)/(pla*trt.n)

#table 3, reconstructing
M<-as.table(rbind(c(2113, 1297),c((2113/85.2*100)-2113,
                                  (1297/95.6*100)-1297)))
dimnames(M) <- list(race = c("white","black"),
party = c("Dominant","Divergent"))
M
chisq.test(M)

M<-as.table(rbind(c(1099, 509),c((1099/44.3*100)-1099,
                                (509/37.5*100)-509)))
dimnames(M) <- list(race = c("MI","noMI"),
                    party = c("Dominant","Divergent"))
M
chisq.test(M)


M<-as.table(rbind(c(249, 102),c((249/10.2*100)-249,
                                  (102/7.6*100)-102)))
dimnames(M) <- list(race = c("CHF","noCHF"),
                    party = c("Dominant","Divergent"))
M
chisq.test(M)
###
##Stephen Senn responders
##http://errorstatistics.com/2014/07/26/s-senn-responder-despondency-myths-of-personalized-medicine-guest-post/
n<-24
set.seed(123)
x<-rnorm(n, 15, 3)
y<-x+rnorm(1,2,2)
id<-sort(rep( seq(1:n),1))
df<-as.data.frame(cbind(id,x, y))

#for plotting limits
l<-floor(min(x,y))
u<-ceiling(max(x,y))
grp1<-15
grp2<-19

#start plotting side by side
par(mfrow=c(1,2))
plot(id ~ x, main="Counterfactual experiment \nIn this thought experiment the \ndifference that the treatment makes \nto each patient is constant",  
     cex.main=.75,
     xlab="Outcome value", ylab="Patient", 
     col='blue'  , pch = grp2,    
       data=df , xlim=c(l,u)
)
points(id ~ y, col='red', pch = grp1)
#join up points
z<-seq(1,n, by=1)  
for(i in 1:length(z)) {
  q<-c(df$x[i], df$y[i])  
  segments( q[1],i, q[2], i, col='pink')
}
# Now the parallel group trial 
# randomly erasing one of the two points for each patient 
# on the left hand pane
# but keeping even no. on placebo and treatment
df1<-df

  j<-sample(id,length(id)/2, replace=F)
  k<-id[!id %in% j]
  df1$x[j]<-NA
  df1$y[k]<-NA

# for(i in 1:length(z)) {
#   ifelse(rbinom(1,1,0.5)==1, df1$x[i]<-NA, df1$y[i]<-NA)  
# }

plot(id ~ x, main="Parallel group trial \nKeep 50% of each grp but remove one \nof the two points for each patient.\nCommon error look at this \nand judge not everyone benefits",
     cex.main=.75, xlab="Outcome value", ylab="Patient", 
     col='blue'  , pch = grp2,    
     data=df1 , xlim=c(l,u) 
)

points(id ~ y, col='red', pch = grp1, data=df1)
#http://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
par(xpd=NA)
legend(par()$usr[3],n*1.5,c("pla", "trt"), 
       pch = c(grp2,grp1), col = c('blue','red')  , bty="y"
)
# legend("topleft", 
#        inset=c(-0.2,0), legend=c("pla", "trt"), pch= c(grp2,grp1),
#        title="Group")
 par(mfrow=c(1,1))
##end
 
# t.test(x,y , paired=T)
#

mu<-apply(df,2,mean)
mu[3]-mu[2] 

mu<-apply(df1,2,mean, na.rm=T)
mu[3]-mu[2] 
t.test(x,y , paired=F, data=df1)
wilcox.test(x , y, data = df1,paired=F, conf.int = T)
mean(df1$x,na.rm=T)-mean(df1$y, na.rm=T)

 
