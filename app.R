#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(shiny) 
library(nlme)
library(VCA)
library(MASS)
require(tidyverse)
require(ggplot2)
library(shinyWidgets)

options(max.print=1000000)
fig.width <- 1375
fig.height <- 550

fig.width2 <- 1375 #1200
fig.height2 <- 730
library(shinythemes)        # more funky looking apps
p1 <- function(x) {formatC(x, format="f", digits=1)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
options(width=100)
#set.seed(12345) #reproducible

pop=1e6
# function to create longitudinal data  

is.even <- function(x){ x %% 2 == 0 }

# Individual response to treatment: is it a valid assumption senn
# 1- pnorm((250-200)/sqrt(100^2+100^2))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(theme = shinytheme("paper"), #https://www.rdocumentation.org/packages/shinythemes/versions/1.1.2
                
       
    

                       setBackgroundColor(
                  color = c( "#2171B5", "#F7FBFF"),
                  gradient = "linear",
                  direction = "bottom"
                ),

            
                
                h3("Responders non responders fallacy - lets power this...Bland ref seen ref."),
                
    
                
                
                
               # shinyUI(pageWithSidebar(
            
                    
                   titlePanel("Hello Shiny!"),
                   
                   
                   
                   sidebarLayout(
                    
                     
                     
                     
                     
                    sidebarPanel( width=3 ,
                      
                    
                     tags$style(type="text/css", ".span8 .well { background-color: #00FFFF; }"),
                      
                      #wellPanel(style = "background: #2171B5",),
                      
                      
                        tags$style(".well {background-color:#b6aebd ;}"), ##ABB0B4AF
                       
                      
      
                      
                      
                      
                        div(p("  I perform a simulation of a randomised control trial demonstrating one reason why it is wrong to analyse arms 
                        of a trial separately aiming to identify responders and non responders. 
                     ")),
                        
                        div(
                            
                          tags$head(
                            tags$style(HTML('#ab1{background-color:orange}'))
                          ),
                          
                          tags$head(
                            tags$style(HTML('#resample{background-color:orange}'))
                          ),
                          
                          
                            actionButton(inputId='ab1', label="R code",   icon = icon("th"), 
                                         onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/biochemistry-and-haematology/master/heam_biochem/app.R', '_blank')"),   
                            actionButton("resample", "Simulate a new sample"),
                            br(), br(),
                            
                            div(strong("Select the parameters using the sliders below"),p(" ")),
                            
                            
                    
                            
                            
                            div(("  
                           xxxxxxxxxxxxxx  ")),
                            br(),

                            sliderInput("power", 
                                        strong("Power"),
                                        min=.50, max=.99, step=.01, value=.9, 
                                        ticks=FALSE),
                            
                            sliderInput("alpha", 
                                        strong("alpha"),
                                        min=.01, max=.2, step=.01, value=.05, 
                                        ticks=FALSE),

                            sliderInput("trt",
                                        strong("treatment effect"),
                                        min=-10, max=10, step=.2, value=-1, ticks=FALSE),
                            
                            sliderInput("pop_mu",
                                        strong("population mean"),
                                        min=-10, max=10, step=1, value=0, ticks=FALSE),
                            
                            sliderInput("pop_sd",
                                        strong("population sd"),
                                        min=1, max=10, step=1, value=4, ticks=FALSE),
                            
                            sliderInput("noise",
                                        strong("random noise"),
                                        min=0, max=4, step=.2, value=2, ticks=FALSE),
                            
                            
                            sliderInput("eligible",
                                        strong("eligible if multiple SD from population mean "),
                                        min=-3, max=3, step=1, value=0, ticks=FALSE),
                   
                            
                            div(p( strong("References:"))),  
                            
                    
                     
                          
                            tags$a(href = "https://en.wikipedia.org/wiki/Anscombe%27s_quartet", "[1] Anscombe's quartet"),
                            div(p(" ")),
                            tags$a(href = "https://en.wikipedia.org/wiki/Comprehensive_metabolic_panel", "[2] Comprehensive metabolic panel"),
                            div(p(" ")),
                            tags$a(href = "https://ggplot2.tidyverse.org/reference/geom_boxplot.html", "[3] Boxplots using ggplot2"),
                            div(p(" ")),
                            tags$a(href = "https://en.wikipedia.org/wiki/Statistical_process_control", "[4] Statistical process control"),
                            div(p(" ")),
                            tags$a(href = "https://twitter.com/f2harrell/status/1220700181496320001", "[5] Purpose of RCT"),
                            div(p(" ")),
                        )
                        
                      
                      
                      
                      
                    ),
                    
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tab panels
                    mainPanel(width=9,
                   
               
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        #    tabsetPanel(type = "tabs", 
                        navbarPage(       
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
                
                            
      
                            
                            
                            tags$style(HTML("
                            .navbar-default .navbar-brand {color: orange;}
                            .navbar-default .navbar-brand:hover {color: blue;}
                            .navbar { background-color: #b6aebd;}
                            .navbar-default .navbar-nav > li > a {color:black;}
                            .navbar-default .navbar-nav > .active > a,
                            .navbar-default .navbar-nav > .active > a:focus,
                            .navbar-default .navbar-nav > .active > a:hover {color: pink;background-color: purple;}
                            .navbar-default .navbar-nav > li > a:hover {color: black;background-color:yellow;text-decoration:underline;}
                            .navbar-default .navbar-nav > li > a[data-value='t1'] {color: red;background-color: pink;}
                            .navbar-default .navbar-nav > li > a[data-value='t2'] {color: blue;background-color: lightblue;}
                            .navbar-default .navbar-nav > li > a[data-value='t3'] {color: green;background-color: lightgreen;}

                   ")),
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end of section to add colour     
                            tabPanel("Plot observed change in order of magnitude", 
                                     #    h2("Plotting the data"),
                                     div(plotOutput("reg.plot3", width=fig.width, height=fig.height)),  
                                     h5("Figure 1 Observed change in each patient in order of magnitude, treated (left) and control (right) arms."),
                                     
                                     h3(" "),
                                     
                                     p(strong("In the data simulation, the ‘true’ value for all treated patients
                                     decreased by constant value. 
                                     The individual differences in change suggested by the ﬁgure are due entirely 
                                     to within-patient variation in baseline and follow-up measurements and regression to the mean.
                                     Left panel, treated group. Observed responders in blue. But **EVERYBODY** responded to the drug **EQUALLY** ! Apparent individual difference is due **ENTIRELY** to 
                                              random within subject error, measurement error and regression to the mean. 
                                              Right panel, control group. Observed responders in blue. But in truth **NO ONE** responded, apparent individual difference is due **ENTIRELY** to random within subject error,
                                              measurement error and regression to the mean.")),
                                     
                                   #  verbatimTextOutput("C"),
                                     
                                   #  DT::dataTableOutput("tablex"),
                                     
                                  # div( verbatimTextOutput("xx")),
                                 #    p(strong("Total sample size:")),
                                #        
                            ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("Plot observed individual change against baseline",
                                     h4("Fxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
                                     div(plotOutput("res.plot", width=fig.width, height=fig.height)),  
                                     h5("Figure 2 Observed individual changes plotted against baseline, treated (left) and control (right) arms. "),         
                                     
                                     
                                     p(strong("The negative slope so often seen in this type of plot can be due entirely to regression to the mean and mathematical coupling. Participants with a relatively high measured value
                                     at baseline will naturally regress towards the mean so that follow-up measurements are lower, and vice versa for participants with a relatively low value
                                  at baseline. This regression to the mean leads to the artefact of a negative correlation between change and initial value (or any other variable that is correlated with the initial value).
                                              ")),
                            ),
                            
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("All plots together", value=3, 
                                    #  h4("xxxxxxxxxxxxxxxxxx"),#
                                     #h6("xxxxxxxxxxxxxxxxxx."),
                                     div(plotOutput("res.plot4", width=fig.width2, height=fig.height2)), 
                                    h5("Figure 3 All plots together. Top indivduals ordered by increasing observed response in treated (left) and control (right) arms. 
                                        Bottom planels show observed response by baseline in treated (left) and control (right) arms. "),         
                            ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("ANCOVA model", value=6, 
                                     h4("Modelling"),
                                     p(strong("xxxxxxxxxxxxxxxxxxxxxx.")),
                                   div( verbatimTextOutput("reg.summary2")),
                                   p(strong("xxxxxxxxxxxxxxxxxxxxxx.")),
                                   div( verbatimTextOutput("reg.summary3")),
                                ) ,
                            
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("Plot of the treatment effect estimates", 
       
                            ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("Data listing", value=3, 
                                     #  h4("Data listing"),
                                     h6("xxxxxxxxxxxxxxxxxxxxxxxxxxx"),
                                     DT::dataTableOutput("table1"),
                                     
                            ) 
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        )
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    )
                    
               ) #
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end tab panels 
                    
             #   )
             #   )
)

server <- shinyServer(function(input, output   ) {
    
    # --------------------------------------------------------------------------
    # ---------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # This is where a new sample is instigated 
    random.sample <- reactive({
        
        # Dummy line to trigger off button-press
        power <- input$power
        alpha <- input$alpha
        foo <-    input$resample
        trt<-     input$trt
        mu <-     input$pop_mu
        sd <-     input$pop_sd
        n <-      pop
        noise <-  input$noise     
        eligible <- input$eligible  
        
        return(list( n=n ,  trt=trt , mu=mu, sd=sd, noise=noise, eligible=eligible, power=power, alpha=alpha )) 
        
    })
    # ---------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # ---------------------------------------------------------------------------
    # n <- 10000
    # power <- .9
    # alpha <- .05
    # beta.treatment <-     -2
    # pop_mu <-     7
    # pop_sd <-     5
    # 
    # noise <-  3
    # ur.eligible <- 0

    # 
    # --------------------------------------------------------------------------
    # ---------------------------------------------------------------------------
    # ---------------------------------------------------------------------------
    make.data <- reactive({
        
        sample <- random.sample()
         
        n <-  sample$n
        noise <-  sample$noise        # add noise (within person var & meas. error) to the baseline & foll. up
        beta.treatment <-  sample$trt #  all trt'd subjects exp same trt effect, so no resp - non responders!!
        alpha <- sample$alpha
        power <-   sample$power
        pop_mu <-  sample$mu    # population mean 
        pop_sd <-  sample$sd   # between person SD
        ur.eligible <- sample$eligible #89
        
        
        N <- round(power.t.test( delta = beta.treatment, sd= pop_sd , 
                                 sig.level= alpha, power= power,
                                 type="two.sample", alternative=c("two.sided"))$n*2)
        
        # eligibility criteria for trial
        y.0true <- rnorm(n, pop_mu, pop_sd)                  # true baseline
        y.0observed <- y.0true + rnorm(n, 0, 1*noise)        # observed baseline 
        
        eligible <- ifelse(y.0observed > ur.eligible, 1, 0)  # 1sd above norm eligible for trial
        treat <- 1*(runif(n)<.5)                             # random treatment allocation
        y.1true <- y.0true + (treat*beta.treatment)          # true follow up, treated only respond
        y.1observed <- y.1true + rnorm(n, 0, 1*noise)        # observed follow up, noise added 
        delta.observed <- y.1observed - y.0observed
        
        d <- data.frame(y.0true, y.0observed, eligible, treat , beta.treatment,
                        y.1true, y.1observed, delta.observed)
        
        # prob that a member of pop observed baseline is eligible
        # pnorm(ur.eligible, mean= pop_mu, sd=sqrt(pop_sd^2 + noise^2))
        # 1- pnorm( (pop_mu - ur.eligible) / sqrt(pop_sd^2+noise^2) )  # z score calc.
        
        trial  <- d[d$eligible==1,]    # select the trial subjects
        
        d <- trial <- trial[1:N,]  # selcet out sample size from the population
        
        return(list(trial=trial,  d=d,  N=N)) 
        
    })

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$summaryx3 <- renderPrint({
      print(make.data()$N)
    }) 

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    fit <- reactive({
        
      d <- make.data()$d
      f0 <- lm(y.1observed ~ y.0observed + treat, d)
      s <- summary(f0)
      ci <- confint(f0)
      
      return(list(s=s, ci=ci, f0=f0 ))
    })     
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$reg.summary <- renderPrint({
       
        return(fit()$f0)
      
    })
    output$reg.summary2 <- renderPrint({
      
      return(fit()$s)
      
    })
    output$reg.summary3 <- renderPrint({
      
      return(fit()$ci)
      
    })
    
    # --------------------------------------------------------------------------
    # ---------------------------------------------------------------------------
    
    output$reg.plot3 <- renderPlot({         
        
        trial <- make.data()$trial
        sample <- random.sample()
        N <- make.data()$N
        
        
        diff <- trial$y.1observed - trial$y.0observed
        mi <-  min( diff)*1.2
        ma <-  max(diff)*1.2
        
        
        stats <- stats()
         A=stats()$A
         AT=stats()$AT 
         C=stats()$C    
         CT=stats()$CT
         AN=stats()$AN
         CN=stats()$CN
        # ---------------------------------------------------------------------------
        par(mfrow=c(1,2))
        
        xup <-  max(table(trial$treat))  # new
  
        trt <- trial[trial$treat==1,]
        trt$diff <- trt$y.1observed - trt$y.0observed
  
        foo <- sort(trt[,"diff"])
        
        foo <- data.frame(foo, col1=NA, col2=NA)
        
        foo$col1 =   ifelse(foo$foo <    trt$beta.treatment, "blue" , "black")         
        foo$col2 =   ifelse(foo$foo >    trt$beta.treatment, "blue" , "black")   
        
        if (trt$beta.treatment <  0) {foo$colz = foo$col1} else {foo$colz = foo$col2}
      
        
        
        # Z <- data.frame(AN=AN, A=A, AT=AT, CN=CN, C=C, CT= CT)
        # names(Z) <- c("N trt","Observed responders trt",  "%" , "N ctrl","Observed responders ctrl" , "%")
        
        
        tex <- paste0("Treated patients \n N= ",AN,", No of responders= ",A," (",AT,"%)")
        
        plot(foo$foo, main=tex,
             ylab= "follow up - baseline", xlab="Individual subjects ordered by observed response", 
             xlim=c(0, xup), ylim=c(mi,ma), #length(trt[,"diff"])
             col=  foo$colz)
        grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
        
        
      
          # 
          # 
          # 
          # plot(foo, main=tex,
          #      ylab= "follow up - baseline", xlab="Individual subjects order by observed response", 
          #      xlim=c(0,1.05*N/2), ylim=c(mi,ma), #length(trt[,"diff"])
          #      col=  ifelse(beta.treatment <  0, col1 , 
          #                   ifelse(beta.treatment >  0, col2 ,    NA ) ))
          #  
         
         
     
        
        
        
        abline(h=0)
        abline(h=input$trt, lty=2)
        # this many were not observed to have reduced response by more than 5
        # wrongly labelled as 'non responders'
        mean(foo > input$trt)*length(foo)   # shown in red
        
        # ---------------------------------------------------------------------------
        
        trt <- trial[trial$treat==0,]
        trt$diff <- trt$y.1observed - trt$y.0observed
        foo <- sort(trt[,"diff"])
        
        foo <- data.frame(foo, col1=NA, col2=NA)
        
        foo$col1 =   ifelse(foo$foo <    trt$beta.treatment, "blue" , "black")         
        foo$col2 =   ifelse(foo$foo >    trt$beta.treatment, "blue" , "black")   
        
        if (trt$beta.treatment <  0) {foo$colz = foo$col1} else {foo$colz = foo$col2}
        
        # tex <- "Individual changes in response in treated arm
        #    Suggested individual differences due entirely to regression to the mean
        #    and random error (within subject and measurement error)"
        
        tex <- paste0("Control patients \n N= ",CN,", No of responders= ",C," (",CT,"%)")
        
        plot(foo$foo, main=tex,
             ylab= "follow up - baseline", xlab="Individual subjects ordered by observed response", 
             xlim=c(0, xup), ylim=c(mi,ma), #length(trt[,"diff"])
             col=  foo$colz)
        grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
        
        abline(h=0)
        abline(h=input$trt, lty=2)
        # this many were not observed to have red uced response by more than 5
        # wrongly labelled as 'non responders'
         mean(foo > input$trt)*length(foo)   # shown in red
        
        par(mfrow=c(1,1))
        # ---------------------------------------------------------------------------
    })
    
    
    # --------------------------------------------------------------------------
    # ---------------------------------------------------------------------------    
    output$res.plot  <- renderPlot({       
        
      sample <- random.sample()
      
        trial <- make.data()$trial
        
        stats <- stats()
        A=stats()$A
        AT=stats()$AT 
        C=stats()$C    
        CT=stats()$CT
        AN=stats()$AN
        CN=stats()$CN
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
 
        trt$col1 =   ifelse(trt$diff <  (sample$trt), "blue" , "black")         
        trt$col2 =   ifelse(trt$diff >  (sample$trt), "blue" , "black")           
    
        
        par(mfrow=c(1,2))
        with(trt, plot(diff ~  y.0observed, 

                  col=  ifelse(beta.treatment <  0, trt$col1 , 
                                   ifelse(beta.treatment >  0, trt$col2 ,    NA )) ,
                       pch=16
                       , xlab="observed baseline",  ylab="follow up - baseline"  ,
         main=paste0("Treatment arm: Individual changes against baseline, observed responders in blue\nPearson's correlation ",cr
                      ," ; treated patients \n N= ",AN,", No of responders= ",A," (",AT,"%)")
                     
                     , cex.main =1.25,
                       ylim=c(mi,ma), xlim=c(mix,max) ))
     
        # 
        # with(trt, abline(lm(diff ~  y.0observed)))
        # with(trt, abline(h=mean(beta.treatment), lty=2))
        # with(trt, abline(h=0, col="red" ))
        
        
        
        with(trt, abline(lm(diff ~  y.0observed), col=c("red"), lty=c(1), lwd=c(2) ) )
        with(trt, abline(h=mean(beta.treatment), col=c("forestgreen"), lty="dashed", lwd=c(2) ) )
        #with(trt, abline(h=0, col="black" , lty=1)) 
        # abline(h=0)
        # 
        grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
        abline(h=0, lwd=c(1))
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        ctr <- trial[trial$treat==0,]
        ctr$diff <- ctr$y.1observed - ctr$y.0observed
    
        cr <- with(ctr, cor.test( diff,   y.0observed, method="pearson"))
        cr$estimate[1][[1]]
        cr$conf.int[1:2]
        cr <- paste0( p2(cr$estimate),", 95%CI (",p2(cr$conf.int[1]),", " ,p2(cr$conf.int[2]), " )")
  
        ctr$col1 =   ifelse(ctr$diff <  (sample$trt), "blue" , "black")         
        ctr$col2 =   ifelse(ctr$diff >  (sample$trt), "blue" , "black")   
        
        with(ctr, plot(diff ~  y.0observed, 
                       col=  ifelse(beta.treatment <  0, ctr$col1 , 
                                    ifelse(beta.treatment >  0, ctr$col2 ,    NA )) ,
                      pch=16
               , xlab="observed baseline",  ylab="follow up - baseline"  ,
              main=paste0("Control arm:  Individual changes against baseline, observed responders in blue\nPearson's correlation ",cr
                         , "; control patients \n N= ",CN,", No of responders= ",C," (",CT,"%)")
                          , cex.main =1.25,
             ylim=c(mi,ma), xlim=c(mix,max) ) ) 
    
        
        with(ctr, abline(lm(diff ~  y.0observed), col=c("red"), lty=c(1), lwd=c(2) ) )
       # with(ctr, abline(h=mean(beta.treatment), col=c("forestgreen"), lty="dashed", lwd=c(1) ) )
      #  with(ctr, abline(h=0, col="black" , lty=4)) 
        
        grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
        abline(h=0, lwd=c(1))
        with(ctr, abline(h=(beta.treatment), col=c("forestgreen"), lty="dashed",  lwd=c(2) ))
        par(mfrow=c(1,1))

    })
    # --------------------------------------------------------------------------
    # ---------------------------------------------------------------------------    
    output$res.plot4 <- renderPlot({       
      
      sample <- random.sample()
      
    
      
      trial <- make.data()$trial
      
      N <- make.data()$N
      
      stats <- stats()
      A=stats()$A
      AT=stats()$AT 
      C=stats()$C    
      CT=stats()$CT
      AN=stats()$AN
      CN=stats()$CN
      
      diff <- trial$y.1observed - trial$y.0observed
      mi <-  min( diff)*1.2
      ma <-  max(diff)*1.2
      
      # ---------------------------------------------------------------------------
      par(mfrow=c(2,2))
      par(bg = 'ivory')
      
      xup <-  max(table(trial$treat))  # new
      trt <- trial[trial$treat==1,]
      trt$diff <- trt$y.1observed - trt$y.0observed
      
      foo <- sort(trt[,"diff"])
      A <- mean(foo < input$trt)*length(foo)   # shown in red
      
      
      foo <- data.frame(foo, col1=NA, col2=NA)
      
      foo$col1 =   ifelse(foo$foo <    trt$beta.treatment, "blue" , "black")         
      foo$col2 =   ifelse(foo$foo >    trt$beta.treatment, "blue" , "black")   
      
      if (trt$beta.treatment <  0) {foo$colz = foo$col1} else {foo$colz = foo$col2}
      
      tex <- "Individual changes in response in treated arm
           Suggested individual differences due entirely to regression to the mean
           and random error (within subject and measurement error)"
      tex <- paste0("Treated patients: N= ",AN,", No of responders= ",A," (",AT,"%)")
      plot(foo$foo, main=tex, 
           ylab= "follow up - baseline", xlab="Individual subjects order by observed response", 
           xlim=c(0, xup), ylim=c(mi,ma), #length(trt[,"diff"])
           col=  foo$colz)
      
      # 
      # 
      # plot(foo, main="Treated arm",
      #      ylab= "follow up - baseline", xlab="Individual subjects order by observed response", 
      #      xlim=c(0,1.05*N/2), ylim=c(mi,ma), #length(trt[,"diff"])
      #      col=ifelse(foo > input$trt, 'black', 'blue') ) #, asp=4)
     # abline(h=0, lty=2)
     # abline(h=input$trt)
    #  with(trt, abline(h=mean(beta.treatment), col=c("forestgreen"), lty=c(1), lwd=c(1) ) )
   #   with(trt, abline(h=0, col="black", lty="dashed")) 
      #with(trt, abline(v=A, col="black", lty="dashed"))
      # this many were not observed to have reduced response by more than 5
      # wrongly labelled as 'non responders'
    #  grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
      
      
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
      
      foo$col1 =   ifelse(foo$foo <    trt$beta.treatment, "blue" , "black")         
      foo$col2 =   ifelse(foo$foo >    trt$beta.treatment, "blue" , "black")   
      
      if (trt$beta.treatment <  0) {foo$colz = foo$col1} else {foo$colz = foo$col2}
      
      tex <- "Individual changes in response in treated arm
           Suggested individual differences due entirely to regression to the mean
           and random error (within subject and measurement error)"
      tex <- paste0("Control patients: N= ",CN,", No of responders= ",C," (",CT,"%)")
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
      
      trt$col1 =   ifelse(trt$diff <  (sample$trt), "blue" , "black")         
      trt$col2 =   ifelse(trt$diff >  (sample$trt), "blue" , "black")           
      
      
    # par(mfrow=c(2,2))
      with(trt, plot(diff ~  y.0observed,
                     
                     col=  ifelse(beta.treatment <  0, trt$col1 , 
                                  ifelse(beta.treatment >  0, trt$col2 ,    NA )) ,
                     
                     
                     pch=16
                     , xlab="observed baseline",  ylab="follow up - baseline"  ,
                     
                     main=paste0("Treatment arm: observed responders in blue\nPearson's correlation ",cr),
                     
                     
                    # main="Treatment arm: Individual changes against baseline, observed responders in blue", 
                     
                     cex.main =1.25,
                     ylim=c(mi,ma), xlim=c(mix,max) ))
      
      
      # par(mfrow=c(2,2))
      # with(trt, plot(diff ~  y.0observed, col=ifelse(diff < sample$trt, 'blue', 'black'), pch=16
      #                , xlab="observed baseline",  ylab="follow up - baseline"  ,
      #                main="Treatment arm: Individual changes against baseline, observed responders in blue", cex.main =1,
      #                ylim=c(mi,ma), xlim=c(mix,max) ))
      # with(trt, abline(lm(diff ~  y.0observed), col=c("red"), lty=c(2), lwd=c(2) ) )
      # with(trt, abline(h=mean(beta.treatment), col=c("forestgreen"), lty=c(1), lwd=c(1) ) )
      # with(trt, abline(h=0, col="black", lty="dashed")) 
      # grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
      
      
      grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
      with(trt, abline(h=0, col="black", lty=1))
      with(trt, abline(lm(diff ~  y.0observed), col=c("red"), lty=c(1), lwd=c(1) ) )
      with(trt, abline(h=(beta.treatment), col=c("forestgreen"), lty=c(2), lwd=c(1) ) )
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      ctr <- trial[trial$treat==0,]
      ctr$diff <- ctr$y.1observed - ctr$y.0observed
      
     # with(trt, cor.test( diff,   y.0observed, method="pearson"))
      
      
      cr <- with(ctr, cor.test( diff,   y.0observed, method="pearson"))
      cr$estimate[1][[1]]
      cr$conf.int[1:2]
      cr <- paste0( p2(cr$estimate),", 95%CI (",p2(cr$conf.int[1]),", " ,p2(cr$conf.int[2]), " )")
     
      # par(bg = 'blue')
      # with(ctr, plot(diff ~  y.0observed, col=ifelse(diff <  sample$trt, 'blue', 'black'), pch=16
      #                , xlab="observed baseline",  ylab="follow up - baseline"  ,
      #                main="Control arm:  Individual changes against baseline, observed responders in blue", cex.main =1,
      #                ylim=c(mi,ma), xlim=c(mix,max) ))
      # with(ctr, abline(lm(diff ~  y.0observed), col=c("red"), lty=c(2), lwd=c(2) ) )
      # with(ctr, abline(h=mean(beta.treatment), col=c("forestgreen"), lty=c(1), lwd=c(1) ) )
      # with(ctr, abline(h=0, col="black", lty="dashed"))
      
      
      ctr$col1 =   ifelse(ctr$diff <  (sample$trt), "blue" , "black")         
      ctr$col2 =   ifelse(ctr$diff >  (sample$trt), "blue" , "black")   
      
      with(ctr, plot(diff ~  y.0observed, 
                     
                     # col=ifelse(diff <  sample$trt, 'blue', 'black'), 
                     col=  ifelse(beta.treatment <  0, ctr$col1 , 
                                  ifelse(beta.treatment >  0, ctr$col2 ,    NA )) ,
                     
                     
                     pch=16
                     , xlab="observed baseline",  ylab="follow up - baseline"  ,
                     
                     main=paste0("Treatment arm: observed responders in blue\nPearson's correlation ",cr),
                     
                     
                     # main="Treatment arm: Individual changes against baseline, observed responders in blue", 
                     
                     cex.main =1.25,
                     #main="Control arm:  Individual changes against baseline, observed responders in blue", cex.main =1,
                     ylim=c(mi,ma), xlim=c(mix,max) ))
    
      
        
       grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
       with(ctr, abline(h=0, col="black", lty=1))
       with(ctr, abline(lm(diff ~  y.0observed), col=c("red"), lty=c(1), lwd=c(1) ) )
       with(ctr, abline(h=(beta.treatment), col=c("forestgreen"), lty=c(2), lwd=c(1) ) )
       
       
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      par(mfrow=c(1,1))
      
      
       
      
    })
    
    
    
    stats <- reactive({
      
      sample <- random.sample()
      
      trial <- make.data()$trial
      
      if (sample$trt < 0) {
      # ---------------------------------------------------------------------------
      N <- nrow(trial)
      trt <- trial[trial$treat==1,]
      trt$diff <- trt$y.1observed - trt$y.0observed
      foo <- sort(trt[,"diff"])
      A <- mean(foo < sample$trt)*length(foo)   # shown in red
      AT <- round(A/length(foo)*100,1)
      AN <- length(foo)
      # ---------------------------------------------------------------------------
      
      trt <- trial[trial$treat==0,]
      trt$diff <- trt$y.1observed - trt$y.0observed
      foo <- sort(trt[,"diff"])
      C <- mean(foo < sample$trt)*length(foo)   # sh
      CT <- round(C/length(foo)*100,1)
      CN = length(foo)
      } else { 
        
        
        N <- nrow(trial)
        trt <- trial[trial$treat==1,]
        trt$diff <- trt$y.1observed - trt$y.0observed
        foo <- sort(trt[,"diff"])
        A <- mean(foo > sample$trt)*length(foo)   # shown in red
        AT <- round(A/length(foo)*100,1)
        AN <- length(foo)
        # ---------------------------------------------------------------------------
        
        trt <- trial[trial$treat==0,]
        trt$diff <- trt$y.1observed - trt$y.0observed
        foo <- sort(trt[,"diff"])
        C <- mean(foo > sample$trt)*length(foo)   # sh
        CT <- round(C/length(foo)*100,1)
        CN = length(foo)
    
      }
      
      
      
      Z <- data.frame(AN=AN, A=A, AT=AT, CN=CN, C=C, CT= CT)
      names(Z) <- c("N trt","Observed responders trt",  "%" , "N ctrl","Observed responders ctrl" , "%")
      rownames(Z) <- NULL 
      # ---------------------------------------------------------------------------
      return(list(A=A, AT=AT, C=C, CT= CT, Z=Z, AN=AN, CN=CN)) 
      
    })
   
  output$A <- renderPrint({
      stats()$A
    }) 
    
    output$C <- renderPrint({
      stats()$C
    }) 
    
    
    
    output$xx <- renderPrint({ 
      
      m  <- stats()$Z
      
          return(m )
      
    })
    
    
    output$tablex <- DT::renderDataTable({

      foo<- stats()$Z

      #namez <- c("true baseline","observed baseline","eligible","treatment group","true treatment effect\n in treated only","
          #        true response","observed response","delta observed")
     # names(foo) <- namez
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
          columns= namez,
          digits=c(2,2,2,2)  )
    })

    
    # ---------------------------------------------------------------------------

    output$table1 <- DT::renderDataTable({
        
        foo<- make.data()$d
   
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
                 columns= namez,   
                            digits=c(2,2,0,0,1,2,2,2)  )
    })
    # --------------------------------------------------------------------------
    # ---------------------------------------------------------------------------
    })

# Run the application 
shinyApp(ui = ui, server = server)