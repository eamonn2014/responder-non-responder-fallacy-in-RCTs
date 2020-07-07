#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls())
library(ggplot2)
 
library(nlme)
library(VCA)
library(MASS)
library(tidyverse)
 # library(shinydashboard)
options(max.print=1000000)
# fig.width <- 1375
# fig.height <- 550
# fig.width2 <- 1375  
# fig.height2 <- 730
# fig.width3 <- 800  
# fig.height3 <- 545
p1 <- function(x) {formatC(x, format="f", digits=1)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
options(width=100)
set.seed(12345) # reproducible

n <- pop <- 1e6 # this is the population size we take sample from
is.even <- function(x){ x %% 2 == 0 } # function to id. odd maybe useful
# Always remember that the purpose of a parallel-group randomized trial is to compare the parallel groups, 
# NOT to look at change from baseline.  Baseline should always be an adjustment covariate (only).

                          
                                  trtx <-beta.treatment <-  trt <- -2.5 
                                  noise <- 1 
                                  mu <-   pop_mu  <- 10 
                                  sd <- pop_sd <-8 
                                  eligible =-5 
                                  senn =-2 
                                  power = .99
                                  alpha=0.01
                                  ur.eligible <-  eligible # 
   
    # ttest power is used to get the sample size
    N <- round(power.t.test( delta = beta.treatment, sd= pop_sd , 
                             sig.level= alpha, power= power,
                             type="two.sample", alternative=c("two.sided"))$n*2)
    
    treat <- 1*(runif(n)<.5)                             # random treatment allocation
    
    # to avoid floating point errors add small amount to reponders  when  noise sd =0 only
    if (noise==0 & beta.treatment > 0 ) {
      
      epsilon =0.00001
      
      y.0observed <- y.0true <- rnorm(n, pop_mu, pop_sd) 
      
      y.1observed <- y.1true <-  y.0true +  treat*beta.treatment + epsilon
      
      delta.observed <- y.1observed - y.0observed   
      
    } else if (noise==0 & beta.treatment< 0 ) {
      
      epsilon =0.00001
      
      y.0observed <- y.0true <- rnorm(n, pop_mu, pop_sd) 
      
      y.1observed <- y.1true <-  y.0true +  treat*beta.treatment - epsilon
      
      delta.observed <- y.1observed - y.0observed   
      
    } else {
      # variable treatment effect!
      # beta.treatment <- runif(n,-4,-1 )  # variation in response  
      # beta.treatment <- sample(-1:-4,n,replace=TRUE )
      
      y.0true <- rnorm(n, pop_mu, pop_sd)                  # true baseline
      
      y.0observed <- y.0true + rnorm(n, 0, 1*noise)        # observed baseline 
      
      y.1true <- y.0true + (treat*beta.treatment)          # true follow up, treated only respond
      
      ##################################################################
      
      y.1observed <- y.1true + rnorm(n, 0, 1*noise)        # observed follow up, noise added 
      
      delta.observed <- y.1observed - y.0observed      
      
    }
    
    eligible <- ifelse(y.0observed > ur.eligible*(pop_mu+pop_sd), 1, 0)  # x sds away from pop mu eligible for trial
    
    ##################################################################
    # diff for baseline
    
    d <- data.frame(y.0true, y.0observed, eligible, treat , beta.treatment,
                    y.1true, y.1observed, delta.observed)
    
    # prob that a member of pop observed baseline is eligible
    # pnorm(ur.eligible, mean= pop_mu, sd=sqrt(pop_sd^2 + noise^2))
    # 1- pnorm( (pop_mu - ur.eligible) / sqrt(pop_sd^2+noise^2) )  # z score calc.
    
    trial  <- d[d$eligible==1,]    # select the trial subjects
    
    d <- trial <- trial[1:N,]      # selext out sample size from the population
    
    
   
    f0 <- lm(y.1observed ~ y.0observed + treat, d)
    s <- summary(f0)
    ci <- confint(f0)
    
 
  
  # https://datascienceplus.com/taking-the-baseline-measurement-into-account-constrained-lda-in-r/
        
 
    
    d$Group <- ifelse(d$treat== 1, "Treated", "Placebo")
    
    fav.col=c("#1A425C", "#8AB63F")
    ggplot(d, aes (x=y.0observed,y=y.1observed, col=Group)) + 
      geom_point() + geom_smooth(method="lm", se=FALSE) + 
      scale_color_manual(values=fav.col) + theme_bw() +
      xlab("Baseline version of response") + 
      ylab("Response")
    
  
   
      if (trt < 0) {    
        
        N <- nrow(trial)
        # ---------------------------------------------------------------------------treated
        # trt rel diff -ve
        trt <- trial[trial$treat==1,]
        trt$diff <- trt$delta.observed      # trt effect          
        foo <- sort(trt[,"diff"])                         # sorted treatment effect
        A <- mean(foo <= trt)*length(foo)          # proportion at follow up less than or equal to trt effect
        AT <- round(A/length(foo)*100,1)                  # %
        AN <- length(foo)                                 # count
        
        T.SENN <-   mean(foo < senn)*length(foo)   # proportion at follow up less than clin rel diff
        TC.SENN <- round(T.SENN/length(foo)*100,1)        # %
        # ---------------------------------------------------------------------------ctrl
        trt <- trial[trial$treat==0,]                     # same for ctrl
        trt$diff <- trt$delta.observed 
        foo <- sort(trt[,"diff"])
        C <- mean(foo <= trt)*length(foo)   # 
        CT <- round(C/length(foo)*100,1)
        CN = length(foo)
        
        C.SENN <- mean(foo < senn)*length(foo)
        CT.SENN <- round(C.SENN/length(foo)*100,1)
        
        
      } else { 
        
        N <- nrow(trial)
        trt <- trial[trial$treat==1,]
        trt$diff <- trt$delta.observed 
        foo <- sort(trt[,"diff"])
        A <- mean(foo > trt)*length(foo)   # 
        AT <- round(A/length(foo)*100,1)
        AN <- length(foo)
        
        T.SENN <- mean(foo < senn)*length(foo)
        TC.SENN <- round(T.SENN/length(foo)*100,1)
        # ---------------------------------------------------------------------------
        trt <- trial[trial$treat==0,]
        trt$diff <-trt$delta.observed 
        foo <- sort(trt[,"diff"])
        C <- mean(foo > trt)*length(foo)   # 
        CT <- round(C/length(foo)*100,1)
        CN = length(foo)
        
        C.SENN <-mean(foo <senn)*length(foo)
        CT.SENN <- round(C.SENN/length(foo)*100,1)
      }
      
      
      
      
      
      
      
      
      
      
  
  # --------------------------------------------------------------------------
  # --------------------------------------------------------------------------
  # --------------------------------------------------------------------------
  # tab 1 plot trt and plot ctrl
 
    # for plotting
    diff <- trial$y.1observed - trial$y.0observed
    mi <-  min(diff)*1.2    # for plotting axis
    ma <-  max(diff)*1.2    # for plotting axis
    
 
    # ---------------------------------------------------------------------------
    
    
    par(mfrow=c(1,2))
    # ---------------------------------------------------------------------------
    xup <-  max(table(trial$treat))  # new
    
    # select treated
    trt <- trial[trial$treat==1,]
    trt$diff <- trt$delta.observed  
    
    foo <- sort(trt[,"diff"])
    # ---------------------------------------------------------------------------
    foo <- data.frame(foo, col1=NA, col2=NA)
    foo$col1 =   ifelse(foo$foo <=    beta.treatment, "blue" , "black")     # -ve trt effect cols   
    foo$col2 =   ifelse(foo$foo >     beta.treatment, "blue" , "black")     # +ve trt effect cols
    # ---------------------------------------------------------------------------
    
    if ( beta.treatment <  0) {
      foo$colz = foo$col1
      tex <- paste0("Treated patients \n N= ",AN,", No of responders= ",A," (",AT,"%), non responders=",AN-A," (",100-AT,"%)")
    } else {
      foo$colz = foo$col2
      tex <- paste0("Treated patients \n N= ",AN,", No of responders= ",A," (",100-AT,"%), non responders=",AN-A," (",AT,"%)")  #AN-A A
    }
    
    plot(foo$foo, main=tex,  
         ylab= "follow up - baseline", xlab="Individual subjects ordered by observed response", 
         xlim=c(0, xup), ylim=c(mi,ma), #length(trt[,"diff"])
         col=  foo$colz)
    grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
    abline(h=0)
    # abline(h=input$trt, lty=2)
    abline(h=(trtx), col=c("forestgreen"), lty="dashed", lwd=c(2) ) 
    
    title(main = "", sub = "Patients observed to respond coloured blue, otherwise black; dashed horizontal line denotes the true treatment effect, treated only",  
          adj=0,cex.sub = 0.75, font.sub = 1, col.sub = "black"
          
    )
    #-------------------------------------------------------------------------
    # ---------------------------------------------------------------------------
    # ---------------------------------------------------------------------------
    trt <- trial[trial$treat==0,]
    trt$diff <- trt$delta.observed  
    foo <- sort(trt[,"diff"])
    # ---------------------------------------------------------------------------
    foo <- data.frame(foo, col1=NA, col2=NA)
    foo$col1 =   ifelse(foo$foo <=  beta.treatment, "blue" , "black")         
    foo$col2 =   ifelse(foo$foo >   beta.treatment, "blue" , "black")   
    # ---------------------------------------------------------------------------
    
    if ( beta.treatment <  0) {foo$colz = foo$col1
    tex <- paste0("Control patients \n N= ",CN,", No of responders= ",C," (",CT,"%), non responders=",CN-C," (",100-CT,"%)")
    } else {
      foo$colz = foo$col2
      tex <- paste0("Control patients \n N= ",CN,", No of responders= ",C," (",CT,"%), non responders=",CN-C," (",100-CT,"%)")   #CN-C 
    }
    # ---------------------------------------------------------------------------
    plot(foo$foo, main=tex,
         ylab= "follow up - baseline", xlab="Individual subjects ordered by observed response", 
         xlim=c(0, xup), ylim=c(mi,ma), #length(trt[,"diff"])
         col=  foo$colz)
    grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
    
    abline(h=0)
    #abline(h=input$trt, lty=2)
    abline(h=(trtx), col=c("forestgreen"), lty="dashed", lwd=c(2) ) 
    title(main = "", sub = "Patients observed to respond coloured blue, otherwise black; dashed horizontal line denotes the true treatment effect, treated only",  
          adj=0,cex.sub = 0.75, font.sub = 1, col.sub = "black"
          
    )
    # ---------------------------------------------------------------------------
    par(mfrow=c(1,1))
 
  
  # --------------------------------------------------------------------------
  # --------------------------------------------------------------------------
  # --------------------------------------------------------------------------
  # tab 2
 
    
    diff <- trial$y.1observed - trial$y.0observed
    mi <-  min( diff)*1.2
    ma <-  max(diff)*1.2
    
    x <- trial$y.0observed
    mix <-  min( x) 
    max <-  max(x) 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    trt <- trial[trial$treat==1,]
    trt$diff <- trt$y.1observed - trt$y.0observed
    
    cr <- with(trt, cor.test( diff,   y.0observed, method="pearson"))
    cr$estimate[1][[1]]
    cr$conf.int[1:2]
    cr <- paste0( p2(cr$estimate),", 95%CI (",p2(cr$conf.int[1]),", " ,p2(cr$conf.int[2]), " )")
    
    # Due to floating point arithmetic we will see the values will slightly differ and will get a val so setting this to NA
    if (noise==0) {  cr <- "NA, 95%CI (NA, NA)"  }
    
    trt$col1 =   ifelse(trt$diff <=  (trtx), "blue" , "black")         
    trt$col2 =   ifelse(trt$diff >   (trtx), "blue" , "black")           
    
    if ( beta.treatment <  0) {
      trt$colz = trt$col1
      tex <- paste0("Treatment arm: Individual changes against baseline, \nPearson's correlation ",cr,"\n N= ",AN,", No of responders= ",A," (",AT,"%), non responders=",AN-A," (",100-AT,"%)")
    } else {
      trt$colz = trt$col2
      tex <- paste0("Treatment arm: Individual changes against baseline, \nPearson's correlation ",cr,"\n N= ",AN,", No of responders= ",A," (",AT,"%), non responders=",AN-A," (",100-AT,"%)")
    }
    
    par(mfrow=c(1,2))
    with(trt, plot(diff ~  y.0observed, 
                   
                   col=  ifelse(beta.treatment <  0, trt$col1 , 
                                ifelse(beta.treatment >  0, trt$col2 ,    NA )) ,
                   
                   pch=16
                   , xlab="observed baseline",  ylab="follow up - baseline"  ,
                   main=tex,
                   cex.main =1.25,
                   ylim=c(mi,ma), xlim=c(mix,max) )) 
    
    with(trt, abline(lm(diff ~  y.0observed), col=c("red"), lty=c(1), lwd=c(2) ) )
    with(trt, abline(h=(beta.treatment), col=c("forestgreen"), lty="dashed", lwd=c(2) ) )
    
    grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
    abline(h=0, lwd=c(1))
    title(main = "", sub = "Patients observed to respond coloured blue, otherwise black; dashed horizontal line denotes the true treatment effect, treated only.",  
          adj=0,cex.sub = 0.75, font.sub = 1, col.sub = "black")
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ctr <- trial[trial$treat==0,]
    ctr$diff <- ctr$y.1observed - ctr$y.0observed
    
    cr <- with(ctr, cor.test( diff,   y.0observed, method="pearson"))
    cr$estimate[1][[1]]
    cr$conf.int[1:2]
    cr <- paste0( p2(cr$estimate),", 95%CI (",p2(cr$conf.int[1]),", " ,p2(cr$conf.int[2]), " )")
    
    # Due to floating point arithmetic we will see the values will slightly differ and will get a val so setting this to NA
    if (noise==0) {  cr <- "NA, 95%CI (NA, NA)"  }
    
    ctr$col1 =   ifelse(ctr$diff <=  (trtx), "blue" , "black")         
    ctr$col2 =   ifelse(ctr$diff >  (trtx), "blue" , "black")   
    
    if ( beta.treatment <  0) {
      ctr$colz = ctr$col1
      tex <- paste0("Control arm: Individual changes against baseline, \nPearson's correlation ",cr,"\n N= ",CN,", No of responders= ",C," (",CT,"%), non responders=",CN-C," (",100-CT,"%)")
    } else {
      ctr$colz = ctr$col2
      tex <- paste0("Control arm: Individual changes against baseline, \nPearson's correlation ",cr,"\n N= ",CN,", No of responders= ",C," (",CT,"%), non responders=",CN-C," (",100-CT,"%)")
    }
    
    with(ctr, plot(diff ~  y.0observed, 
                   col=  ifelse(beta.treatment <  0, ctr$col1 , 
                                ifelse(beta.treatment >  0, ctr$col2 ,    NA )) ,
                   pch=16
                   , xlab="observed baseline",  ylab="follow up - baseline"  ,
                   main=tex, #paste0("Control arm:  Individual changes against baseline, observed responders in blue\nPearson's correlation ",cr
                   # , "; control patients \n N= ",CN,", No of responders= ",C," (",CT,"%)")
                   cex.main =1.25,
                   ylim=c(mi,ma), xlim=c(mix,max) ) ) 
    
    with(ctr, abline(lm(diff ~  y.0observed), col=c("red"), lty=c(1), lwd=c(2) ) )
    
    grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
    abline(h=0, lwd=c(1))
    with(ctr, abline(h=(beta.treatment), col=c("forestgreen"), lty="dashed",  lwd=c(2) ))
    
    title(main = "", sub = "Red line is linear regression line of best fit.",  
          adj=0,cex.sub = 0.75, font.sub = 1, col.sub = "black")
    
    par(mfrow=c(1,1))
 
  # --------------------------------------------------------------------------
  # --------------------------------------------------------------------------
  # --------------------------------------------------------------------------   
  # 3rd tab
 
    
    diff <- trial$y.1observed - trial$y.0observed
    mi <-  min( diff)*1.2
    ma <-  max(diff)*1.2
    beta.treatment <-  trtx 
    # ---------------------------------------------------------------------------
    par(mfrow=c(2,2))
    # par(bg = 'ivory')
    
    xup <-  max(table(trial$treat))  # new
    trt <- trial[trial$treat==1,]
    trt$diff <- trt$y.1observed - trt$y.0observed
    
    foo <- sort(trt[,"diff"])
    A <- mean(foo < trtx)*length(foo)   # shown in red
    
    
    foo <- data.frame(foo, col1=NA, col2=NA)
    
    foo$col1 =   ifelse(foo$foo <=    trt$beta.treatment, "blue" , "black")         
    foo$col2 =   ifelse(foo$foo >    trt$beta.treatment, "blue" , "black")   
    
    if (beta.treatment <  0) {foo$colz = foo$col1} else {foo$colz = foo$col2}
    # 
    # tex <- "Individual changes in response in treated arm
    #      Suggested individual differences due entirely to regression to the mean
    #      and random error (within subject and measurement error)"
    # tex <- paste0("Treated patients: N= ",AN,", No of responders= ",A," (",AT,"%)")
    
    if ( beta.treatment <  0) {
      foo$colz = foo$col1
      tex <- paste0("Treated patients, responders coloured blue \n N= ",AN,", No of responders= ",A," (",AT,"%), non responders=",AN-A," (",100-AT,"%)")
    } else {
      foo$colz = foo$col2
      tex <- paste0("Treated patients, responders coloured blue \n N= ",AN,", No of responders= ",AN-A," (",100-AT,"%), non responders=",A," (",AT,"%)")
    }
    
    plot(foo$foo, main=tex, 
         ylab= "follow up - baseline", xlab="Individual subjects order by observed response", 
         xlim=c(0, xup), ylim=c(mi,ma), #length(trt[,"diff"])
         col=  foo$colz)
    
    grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
    with(trt, abline(v=A, col="black", lty="dashed"))
    with(trt, abline(h=0, col="black", lty=1))
    with(trt, abline(h=(beta.treatment), col=c("forestgreen"), lty=c(2), lwd=c(1) ) )
    # ---------------------------------------------------------------------------
    trt <- trial[trial$treat==0,]
    trt$diff <- trt$y.1observed - trt$y.0observed
    
    foo <- sort(trt[,"diff"])
    C <- mean(foo < input$trt)*length(foo)   # shown in red
    
    foo <- data.frame(foo, col1=NA, col2=NA)
    
    foo$col1 =   ifelse(foo$foo <=    trt$beta.treatment, "blue" , "black")          
    foo$col2 =   ifelse(foo$foo >    trt$beta.treatment, "blue" , "black")   
    
    #if (trt$beta.treatment <  0) {foo$colz = foo$col1} else {foo$colz = foo$col2}
    
    if ( beta.treatment <  0) {foo$colz = foo$col1
    tex <- paste0("Control patients, responders coloured blue\n N= ",CN,", No of responders= ",C," (",CT,"%), non responders=",CN-C," (",100-CT,"%)")
    } else {
      foo$colz = foo$col2
      tex <- paste0("Control patients, responders coloured blue\n N= ",CN,", No of responders= ",CN-C," (",CT,"%), non responders=",C," (",100-CT,"%)") 
    }
    
    plot(foo$foo, main=tex,
         ylab= "follow up - baseline", xlab="Individual subjects order by observed response", 
         xlim=c(0, xup), ylim=c(mi,ma), #length(trt[,"diff"])
         col=  foo$colz)
    
    grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
    with(trt, abline(v=C, col="black", lty="dashed"))
    with(trt, abline(h=0, col="black", lty=1))
    with(trt, abline(h=(beta.treatment), col=c("forestgreen"), lty=c(2), lwd=c(1) ) )
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    trial <- make.data()$trial
    
    diff <- trial$y.1observed - trial$y.0observed
    mi <-  min( diff)*1.2
    ma <-  max(diff)*1.2
    
    x <- trial$y.0observed
    mix <-  min( x) 
    max <-  max(x) 
    
    trt <- trial[trial$treat==1,]
    trt$diff <- trt$y.1observed - trt$y.0observed
    
    cr <- with(trt, cor.test( diff,   y.0observed, method="pearson"))
    cr$estimate[1][[1]]
    cr$conf.int[1:2]
    cr <- paste0( p2(cr$estimate),", 95%CI (",p2(cr$conf.int[1]),", " ,p2(cr$conf.int[2]), " )")
    
    # Due to floating point arithmetic we will the values will slightly differ and will get a val so setting this to NA
    if (noise==0) {  cr <- "NA, 95%CI (NA, NA)"  }
    
    trt$col1 =   ifelse(trt$diff <  (trtx), "blue" , "black")         
    trt$col2 =   ifelse(trt$diff >  (trtx), "blue" , "black")           
    
    if ( beta.treatment <  0) {
      foo$colz = foo$col1
      tex <- paste0("Treatment arm: Individual changes against baseline, \nPearson's correlation ",cr,"\n N= ",AN,", No of responders= ",A," (",AT,"%), non responders=",AN-A," (",100-AT,"%)")
    } else {
      foo$colz = foo$col2
      tex <- paste0("Treatment arm: Individual changes against baseline, \nPearson's correlation ",cr,"\n N= ",AN,", No of responders= ",AN-A," (",100-AT,"%), non responders=",A," (",AT,"%)")
    }
    
    with(trt, plot(diff ~  y.0observed,
                   
                   col=  ifelse(beta.treatment <=  0, trt$col1 , 
                                ifelse(beta.treatment >  0, trt$col2 ,    NA )) ,
                   
                   
                   pch=16
                   , xlab="observed baseline",  ylab="follow up - baseline"  ,
                   
                   main=tex, #paste0("Treatment arm: observed responders in blue\nPearson's correlation ",cr),
                   
                   cex.main =1.25,
                   ylim=c(mi,ma), xlim=c(mix,max) ))
    
    grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
    with(trt, abline(h=0, col="black", lty=1))
    with(trt, abline(lm(diff ~  y.0observed), col=c("red"), lty=c(1), lwd=c(1) ) )
    with(trt, abline(h=(beta.treatment), col=c("forestgreen"), lty=c(2), lwd=c(1) ) )
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    ctr <- trial[trial$treat==0,]
    ctr$diff <- ctr$y.1observed - ctr$y.0observed
    cr <- with(ctr, cor.test( diff,   y.0observed, method="pearson"))
    cr$estimate[1][[1]]
    cr$conf.int[1:2]
    cr <- paste0( p2(cr$estimate),", 95%CI (",p2(cr$conf.int[1]),", " ,p2(cr$conf.int[2]), " )")
    
    # Due to floating point arithmetic we will the values will slightly differ and will get a val so setting this to NA
    if (noise==0) {  cr <- "NA, 95%CI (NA, NA)"  }
    
    ctr$col1 =   ifelse(ctr$diff <  (trtx), "blue" , "black")         
    ctr$col2 =   ifelse(ctr$diff >  (trtx), "blue" , "black")   
    
    if ( beta.treatment <  0) {
      ctr$colz = foo$col1
      tex <- paste0("Control arm: Individual changes against baseline, \nPearson's correlation ",cr,"\n N= ",CN,", No of responders= ",C," (",CT,"%), non responders=",CN-C," (",100-CT,"%)")
    } else {
      ctr$colz = foo$col2
      tex <- paste0("Control arm: Individual changes against baseline, \nPearson's correlation ",cr,"\n N= ",CN,", No of responders= ",CN-C," (",CT,"%), non responders=",C," (",100-CT,"%)")
    }
    
    with(ctr, plot(diff ~  y.0observed, 
                   
                   col=  ifelse(beta.treatment <=  0, ctr$col1 , 
                                ifelse(beta.treatment >  0, ctr$col2 ,    NA )) ,
                   
                   pch=16
                   , xlab="observed baseline",  ylab="follow up - baseline"  ,
                   
                   main=tex, #paste0("Treatment arm: observed responders in blue\nPearson's correlation ",cr),
                   
                   cex.main =1.25,
                   ylim=c(mi,ma), xlim=c(mix,max) ))
    
    grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
    with(ctr, abline(h=0, col="black", lty=1))
    with(ctr, abline(lm(diff ~  y.0observed), col=c("red"), lty=c(1), lwd=c(1) ) )
    with(ctr, abline(h=(beta.treatment), col=c("forestgreen"), lty=c(2), lwd=c(1) ) )
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    par(mfrow=c(1,1))
    
 
  # --------------------------------------------------------------------------
  # --------------------------------------------------------------------------
  # --------------------------------------------------------------------------
  # fith tab
   
    diff <- trial$y.1observed - trial$y.0observed
    mi <-  min( diff)*1.2
    ma <-  max(diff)*1.2
    
 
    xup <-  max(table(trial$treat))  # new
    
    trt <- trial[trial$treat==1,]
    trt$diff <- trt$y.1observed - trt$y.0observed
    
    foo <- sort(trt[,"diff"])
    
    foo <- data.frame(foo, col1=NA, col2=NA)
    
    foo$col1 =   ifelse(foo$foo <=    senn, "blue" , "black")
    foo$col2 =   ifelse(foo$foo >     senn, "blue" , "black")
    
    if ( senn <  0) {
      foo$colz = foo$col1
      tex <- paste0("Treated patients \n N= ",AN,", No of responders= ",T.SENN," (",TC.SENN,"%), non responders=",AN-T.SENN," (",100-TC.SENN,"%)")
    } else {
      foo$colz = foo$col2
      tex <- paste0("Treated patients \n N= ",AN,", No of responders= ",AN-T.SENN," (",100-TC.SENN,"%), non responders=",T.SENN," (",TC.SENN,"%)")
    }
    par(mfrow=c(1,2))
    plot(foo$foo, main=tex,
         ylab= "follow up - baseline", xlab="Individual subjects ordered by observed response",
         xlim=c(0, xup), ylim=c(mi,ma), #length(trt[,"diff"])
         col=  foo$colz)
    grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
    
    abline(h=0)
    abline(h=trtx, lty=2)
    abline(h=senn, lty=2, col="blue")
    title(main = "", sub = "Patients observed to respond coloured blue, otherwise black; blue dashed line denotes clincal relevant difference",  
          adj=0,cex.sub = 0.75, font.sub = 1, col.sub = "black"
          
    )
    # ---------------------------------------------------------------------------
    
    trt <- trial[trial$treat==0,]
    trt$diff <- trt$y.1observed - trt$y.0observed
    foo <- sort(trt[,"diff"])
    
    foo <- data.frame(foo, col1=NA, col2=NA)
    
    foo$col1 =   ifelse(foo$foo <=    senn, "blue" , "black")
    foo$col2 =   ifelse(foo$foo >     senn, "blue" , "black")
    
    if ( senn <  0) {foo$colz = foo$col1
    tex <- paste0("Control patients \n N= ",CN,", No of responders= ",C.SENN," (",CT.SENN,"%), non responders=",CN-C.SENN," (",100-CT.SENN,"%)")
    } else {
      foo$colz = foo$col2
      tex <- paste0("Control patients \n N= ",CN,", No of responders= ",CN-C.SENN," (",100-CT.SENN,"%), non responders=",C.SENN," (",CT.SENN,"%)")
    }
    
    plot(foo$foo, main=tex,
         ylab= "follow up - baseline", xlab="Individual subjects ordered by observed response",
         xlim=c(0, xup), ylim=c(mi,ma), #length(trt[,"diff"])
         col=  foo$colz)
    grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
    
    abline(h=0)
    abline(h=trtx, lty=2)
    abline(h=senn, lty=2, col="blue")
    title(main = "", sub = "Patients observed to respond coloured blue, otherwise black; black dashed line the true trt effect, which should only manifest in the treated only",  
          adj=0,cex.sub = 0.75, font.sub = 1, col.sub = "black"
          
    )
    
    par(mfrow=c(1,1))
    # ---------------------------------------------------------------------------
  
  # --------------------------------------------------------------------------
  # --------------------------------------------------------------------------
 
    
    if (beta.treatment < 0 & senn > 0 ) {
      
      res <-  pnorm( (beta.treatment-senn)/ sqrt(noise^2+noise^2)    )
      res2 <- pnorm( (0-senn)/ sqrt(noise^2+noise^2)    )
      
    }  else if (beta.treatment < 0 & senn <  0 ) {
      
      res <-  1- pnorm( (beta.treatment-senn)/ sqrt(noise^2+noise^2)    )
      res2 <- 1- pnorm( (0-senn)/ sqrt(noise^2+noise^2)    )
      
    }  else  if (beta.treatment > 0 & senn < 0 ) {
      
      res <-  1- pnorm( (beta.treatment-senn)/ sqrt(noise^2+noise^2)    )
      res2 <- 1- pnorm( (0-senn)/ sqrt(noise^2+noise^2)    )
      
      # beta.treatment > 0 & senn > 0 
    } else  {
      
      res <-   pnorm( (beta.treatment-senn)/ sqrt(noise^2+noise^2)    )
      res2 <-  pnorm( (0-senn)/ sqrt(noise^2+noise^2)    )
    }  
    
    
 
   
    
    
    if (trtx < 0) {    
      
      N <- nrow(trial)
      # ---------------------------------------------------------------------------treated
      # trt rel diff -ve
      trt <- trial[trial$treat==1,]
      trt$diff <- trt$delta.observed      # trt effect          
      foo <- sort(trt[,"diff"])                         # sorted treatment effect
      A <- mean(foo <= trtx)*length(foo)          # proportion at follow up less than or equal to trt effect
      AT <- round(A/length(foo)*100,1)                  # %
      AN <- length(foo)                                 # count
      
      T.SENN <-   mean(foo < senn)*length(foo)   # proportion at follow up less than clin rel diff
      TC.SENN <- round(T.SENN/length(foo)*100,1)        # %
      # ---------------------------------------------------------------------------ctrl
      trt <- trial[trial$treat==0,]                     # same for ctrl
      trt$diff <- trt$delta.observed 
      foo <- sort(trt[,"diff"])
      C <- mean(foo <= trt)*length(foo)   # 
      CT <- round(C/length(foo)*100,1)
      CN = length(foo)
      
      C.SENN <- mean(foo < senn)*length(foo)
      CT.SENN <- round(C.SENN/length(foo)*100,1)
      
      
    } else { 
      
      N <- nrow(trial)
      trt <- trial[trial$treat==1,]
      trt$diff <- trt$delta.observed 
      foo <- sort(trt[,"diff"])
      A <- mean(foo > trt)*length(foo)   # 
      AT <- round(A/length(foo)*100,1)
      AN <- length(foo)
      
      T.SENN <- mean(foo < senn)*length(foo)
      TC.SENN <- round(T.SENN/length(foo)*100,1)
      # ---------------------------------------------------------------------------
      trt <- trial[trial$treat==0,]
      trt$diff <-trt$delta.observed 
      foo <- sort(trt[,"diff"])
      C <- mean(foo > trt)*length(foo)   # 
      CT <- round(C/length(foo)*100,1)
      CN = length(foo)
      
      C.SENN <-mean(foo < senn)*length(foo)
      CT.SENN <- round(C.SENN/length(foo)*100,1)
    }
    
     
    
    require(nlme)
    # LMM approach
    m1 <- lme(delta.observed~ treat + y.0observed,
              random=~1|treat , data=d, method="REML",
              weights = varIdent(form = ~1 | treat))
    
    m0 <-lme(delta.observed~ treat + y.0observed,
             random=~1|treat , data=d, method="REML")
    
   
    m2 <- anova(m1,m0) # are the trt ctr interindividual variation in response different?
   
 
  # --------------------------------------------------------------------------- 
  # ---------------------------------------------------------------------------
  require(data.table)
    
    foo<- d
    
    namez <- c("true baseline","observed baseline","eligible","treatment group","true treatment effect\n in treated only","
                  true response","observed response","delta observed")
    names(foo) <- namez
    rownames(foo) <- NULL
    library(DT)
    
    datatable(foo,   
              
              rownames = TRUE,
              #           
              options = list(
                searching = TRUE,
                pageLength = 15,
                paging=TRUE,
                lengthMenu = FALSE ,
                lengthChange = FALSE,
                autoWidth = FALSE,
                #  colReorder = TRUE,
                #deferRender = TRUE,
                # scrollY = 200,
                scroller = T
              ))  %>%
      
      formatRound(
        columns= c("true baseline","observed baseline", "true treatment effect\n in treated only","
                  true response","observed response","delta observed"), 
        digits=4 )
    
   