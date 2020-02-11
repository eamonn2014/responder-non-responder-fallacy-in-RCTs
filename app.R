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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(theme = shinytheme("journal"), #https://www.rdocumentation.org/packages/shinythemes/versions/1.1.2
                
                #paper
    

                       setBackgroundColor(
                  color = c( "#2171B5", "#F7FBFF"),
                  gradient = "linear",
                  direction = "bottom"
                ),

            
                
                h2("Responders, non responders fallacy in parallel RCTs"),
                
    
                h4("  We perform a simulation of a randomised control trial demonstrating one reason why it is wrong to analyse arms 
                of a trial separately aiming to identify responders and non responders. 'The essential feature of a randomised trial is the comparison between groups. Within group analyses do
                not address a meaningful question: the question is not whether there
                is a change from baseline, but whether any change is greater in one group than the other [1].'
              "), 
                
                h3("  "), 
               # shinyUI(pageWithSidebar(
            
                    
              #     titlePanel("Hello Shiny!"),
                   
                   
                   
                   sidebarLayout(
                    
                     
                     
                     
                     
                    sidebarPanel( width=3 ,
                      
                    
                     tags$style(type="text/css", ".span8 .well { background-color: #00FFFF; }"),
                      
                      #wellPanel(style = "background: #2171B5",),
                      
                      
                        tags$style(".well {background-color:#b6aebd ;}"), ##ABB0B4AF
                       
                      
      
                      
                      
                      
                        #div(p("
                        h4("
                        The first slider sets the power and the next alpha level, so we can power the trial as we wish. The next slider is the 'Treatment effect'. 
                        All patients in the treatment arm are given this effect. So a constant treatment is given to ALL in the treated group.
                        Similarly NO ONE in the control group receives any treatment effect. The next two sliders are the 'Population mean' and 'Population sd', this sd is the between 
                        person variation. The 'Random noise' slider is a term representing the within person variation and measurement variation. The next slider imposes if desired
                        an inclusion criteria based on the response distribution."),
                        
                        
                        
                        
                        
                        
                     #")),
                        
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
                            
                            #div(strong("Select the parameters using the sliders below"),p(" ")),
                            
                
                           # div((" ")),
                            #br(),

                            sliderInput("power", 
                                        h5("pPower"),
                                        min=.80, max=.99, step=.01, value=.99, 
                                        ticks=FALSE),
                            
                            sliderInput("alpha", 
                                        h5("Alpha"),
                                        min=.01, max=.2, step=.01, value=.01, 
                                        ticks=FALSE),

                            sliderInput("trt",
                                        h5("Treatment effect"),
                                        min=-10, max=10, step=.1, value=-2.5, ticks=FALSE),
                            
                            sliderInput("pop_mu",
                                        h5("Population mean"),
                                        min=-10, max=10, step=1, value=10, ticks=FALSE),
                            
                            sliderInput("pop_sd",
                                        h5("Population sd"),
                                        min=1, max=10, step=1, value=8, ticks=FALSE),
                            
                            sliderInput("noise",
                                        h5("Random noise"),
                                        min=0, max=4, step=.2, value=1, ticks=FALSE),
                            
                            
                            sliderInput("eligible",
                                        h5("Eligible, if patient is > this many SDs from population mean "),
                                        min=-5, max=5, step=1, value=-5, ticks=FALSE),
                   
                          # sliderInput("senn",
                          #             strong("Clinical relevant difference"),
                          #             min=-10, max=10, step=.1, value=-2, ticks=FALSE),
                            div(h5("References:")),  
                            
                    
                     
                          
                            tags$a(href = "https://www.bmj.com/content/bmj/342/bmj.d561.full.pdf", "[1] Comparisons within randomised groups can be very misleading"),
                            div(p(" ")),
                            tags$a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC524113/pdf/bmj32900966.pdf", "[2] Individual response to treatment: is it a valid assumption?"),
                            div(p(" ")),
                            tags$a(href = "https://www.youtube.com/watch?v=uiCd9m6tmt0&feature=youtu.be", "[3] Professor George Davey Smith - Some constraints on the scope and potential of personalised medicine"),
                            div(p(" ")),
                             tags$a(href = "https://physoc.onlinelibrary.wiley.com/doi/epdf/10.1113/EP085070", "[4] True and false interindividual differencesin the physiological response to an intervention"),
                             div(p(" ")),
                            # tags$a(href = "https://twitter.com/f2harrell/status/1220700181496320001", "[5] Purpose of RCT"),
                            # div(p(" ")),
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
                            tabPanel("Plot change in order of magnitude", 
                                     #    h2("Plotting the data"),
                                     div(plotOutput("reg.plot3", width=fig.width, height=fig.height)),  
                                     h4("Figure 1 Observed change in each patient in order of magnitude, blue observed 'responders'. Treated (left) and control arm (right)."),
                                     
                                     h3(" "),
                                     
                                     
                                     p(strong("It can be seen that the response value of some patients seems to reduce markedly 
                                     in response to the intervention, whereas for other patients it seems to remain unchanged or even increase at follow-up")),
                                     
                                     p(strong("In the simulation, the ‘true’ value for all treated patients
                                     changed by a constant value, indicated by the dashed horizontal line (determined by the 'treatment effect' slider).
                                     The left panel are the treated patients only, with observed 'responders' in blue.
                                     But in truth **EVERYBODY** responded to the drug **EQUALLY** ! ")),
                                
                                     p(strong("It can also be seen that the response value of some patients seems to reduce markedly 
                                     in the control arm, whereas for other patients it seems to remain unchanged or even increase at follow-up")),
                                             p(strong("The right panel is the control group. Observed responders in blue. 
                                              But in truth **NO ONE** responded. Apparent individual difference is due **ENTIRELY** to random within subject error,
                                              measurement error and regression to the mean. Slide the 'random noise' to zero to see.")),
                                     
                                   #  verbatimTextOutput("C"),
                                     
                                   #  DT::dataTableOutput("tablex"),
                                     
                                  # div( verbatimTextOutput("xx")),
                                 #    p(strong("Total sample size:")),
                                #        
                            ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("Plot individual change against baseline",
                                     #h4("Fxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
                                     div(plotOutput("res.plot", width=fig.width, height=fig.height)),  
                                     h4("Figure 2 Observed individual changes plotted against baseline, treated (left) and control (right) arms. "),         
                                     
                                     
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
                                    h4("Figure 3 All plots together. Top, indivduals ordered by increasing observed response in treated (left) and control (right) arms. 
                                        Bottom planels show observed response by baseline in treated (left) and control (right) arms. "),         
                            ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("ANCOVA model", value=6, 
                                    # h4("Modelling"),
                                     h4("Here we estimate the treatment effect with a linear model."),
                                   div( verbatimTextOutput("reg.summary2")),
                                   p(strong("95% CIs")),
                                   div( verbatimTextOutput("reg.summary3")),
                                ) ,
                            
                            
                            tabPanel("Analyse the variance!", value=6, 
                                                          h4("Fisher in a letter on this topic in 1938 said to look at the variance in the outcome [3]. 
                                        Is there any evidence against the null hypothesis that the variance in the outcome in the trial arms differ? 
                                        The P-Value testing this hypothesis will, the vast majority of the time, not reject the null hypothesis, 
                                        as it should, given that the true magnitude of response in the simulation is constant for all 
                                        patients randomised to the treated arm and constant in the control arm (zero). 
                                        This result provides information that any apparant response differences are negligible 
                                        and any analysis of interindividual response is unwarranted."),
                                      div( verbatimTextOutput("reg.lmm0")),
                                      div( verbatimTextOutput("reg.lmm1")),
                                     div( verbatimTextOutput("reg.lmm2")),
                            
                            ) ,
                            
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("Clinically relevant difference", 
                                     div(plotOutput("reg.plotx", width=fig.width, height=fig.height)),  
                                     fluidRow(
                                       column(12,
                                              sliderInput("senn",
                                                          strong("Clinical relevant difference"),
                                                          min=-10, max=10, step=.1, value=-2, ticks=FALSE))
                                     ),
                                     h4("Figure 4 Observed individual changes plotted against baseline, treated (left) and control (right) arms incorporating a clinical relevant difference. "),         
                                     
                                     p(strong(" ")),
                                     p(strong("We duplicate Stephen Senn's example [2], but using a simulated dataset (one realisation). We can calulate 
                                              the proportion of treated who will fail to respond analytically by 1- pnorm((2.5-2)/sqrt(1^2+1^2))= 0.36, see left plot.
                                              The blue dashed line defines the clinically relevant differnece. The black dashed line the constant treatment effect applied 
                                              to EVERYONE in the treated group. Blue circles denote the observed responders.")),
                                     
                                     p(strong("(To duplicate Stephen Senn's paper the default values selected are high power and low alpha.
                                              Select 'treatment effect' of -2.5, 'population mean' and 'population SD' do not matter, set 'random noise' to 1, and 
                                              eligibilty to -5, drop this down so that the vast majority of patients are included, finally set the 'clinically relvant difference' on the above slider to -2.)"
                                               )),
                                  
                                     
                                     
                                     
                            ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("Data", value=3, 
                                     #  h4("Data listing"),
                                     #h6("A list of the data"),
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
        SENN <- input$senn
        
        
        
        return(list( n=n ,  trt=trt , mu=mu, sd=sd, noise=noise, eligible=eligible, power=power, alpha=alpha, SENN =SENN )) 
        
    })
  
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
        
       # beta.treatment <- runif(n,-4,-1 )  # variation in response  
        
        # eligibility criteria for trial
        y.0true <- rnorm(n, pop_mu, pop_sd)                  # true baseline
        y.0observed <- y.0true + rnorm(n, 0, 1*noise)        # observed baseline 
        
        eligible <- ifelse(y.0observed > ur.eligible*(pop_mu+pop_sd), 1, 0)  # sd away from norm eligible for trial
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
         T.SENN =stats()$T.SENN
         C.SENN =stats()$C.SENN
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
      
 
        
        tex <- paste0("Treated patients \n N= ",AN,", No of responders= ",A," (",AT,"%)")
        
        plot(foo$foo, main=tex,
             ylab= "follow up - baseline", xlab="Individual subjects ordered by observed response", 
             xlim=c(0, xup), ylim=c(mi,ma), #length(trt[,"diff"])
             col=  foo$colz)
        grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
        

        
        
        
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

        tex <- paste0("Control patients \n N= ",CN,", No of responders= ",C," (",CT,"%)")
        
        plot(foo$foo, main=tex,
             ylab= "follow up - baseline", xlab="Individual subjects ordered by observed response", 
             xlim=c(0, xup), ylim=c(mi,ma), #length(trt[,"diff"])
             col=  foo$colz)
        grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
        
        abline(h=0)
        abline(h=input$trt, lty=2)

        
        par(mfrow=c(1,1))
        # ---------------------------------------------------------------------------
    })
    
    
    # --------------------------------------------------------------------------
    output$reg.plotx <- renderPlot({         
      
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
      T.SENN =stats()$T.SENN
      C.SENN =stats()$C.SENN
      TC.SENN =stats()$TC.SENN
      CT.SENN =stats()$CT.SENN
      # ---------------------------------------------------------------------------
      par(mfrow=c(1,2))
      
      xup <-  max(table(trial$treat))  # new
      
      trt <- trial[trial$treat==1,]
      trt$diff <- trt$y.1observed - trt$y.0observed
      
      foo <- sort(trt[,"diff"])
      
      foo <- data.frame(foo, col1=NA, col2=NA)
      
      foo$col1 =   ifelse(foo$foo <    sample$SENN, "blue" , "black")         
      foo$col2 =   ifelse(foo$foo >     sample$SENN, "blue" , "black")   
      
      if ( sample$SENN <  0) {foo$colz = foo$col1} else {foo$colz = foo$col2}

      
      tex <- paste0("Treated patients \n N= ",AN,", No of responders= ",T.SENN," (",TC.SENN,"%), non responders=",AN-T.SENN," (",100-TC.SENN,"%)")
      
      plot(foo$foo, main=tex,
           ylab= "follow up - baseline", xlab="Individual subjects ordered by observed response", 
           xlim=c(0, xup), ylim=c(mi,ma), #length(trt[,"diff"])
           col=  foo$colz)
      grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
 
      
      
      
      
      abline(h=0)
      abline(h=input$trt, lty=2)
      abline(h=input$senn, lty=2, col="blue")

      
      # ---------------------------------------------------------------------------
      
      trt <- trial[trial$treat==0,]
      trt$diff <- trt$y.1observed - trt$y.0observed
      foo <- sort(trt[,"diff"])
      
      foo <- data.frame(foo, col1=NA, col2=NA)
      
      foo$col1 =   ifelse(foo$foo <     sample$SENN, "blue" , "black")         
      foo$col2 =   ifelse(foo$foo >     sample$SENN, "blue" , "black")   
      
      if ( sample$SENN <  0) {foo$colz = foo$col1} else {foo$colz = foo$col2}
      
     
      tex <- paste0("Control patients \n N= ",CN,", No of responders= ",C.SENN," (",CT.SENN,"%), non responders=",CN-C.SENN," (",100-CT.SENN,"%)")
      
      plot(foo$foo, main=tex,
           ylab= "follow up - baseline", xlab="Individual subjects ordered by observed response", 
           xlim=c(0, xup), ylim=c(mi,ma), #length(trt[,"diff"])
           col=  foo$colz)
      grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
      
      abline(h=0)
      abline(h=input$trt, lty=2)
      abline(h=input$senn, lty=2, col="blue")
 
      
      par(mfrow=c(1,1))
      # ---------------------------------------------------------------------------
    })
    
    
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

        with(trt, abline(lm(diff ~  y.0observed), col=c("red"), lty=c(1), lwd=c(2) ) )
        with(trt, abline(h=mean(beta.treatment), col=c("forestgreen"), lty="dashed", lwd=c(2) ) )
        
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
     
        ctr$col1 =   ifelse(ctr$diff <  (sample$trt), "blue" , "black")         
      ctr$col2 =   ifelse(ctr$diff >  (sample$trt), "blue" , "black")   
      
      with(ctr, plot(diff ~  y.0observed, 
                     
                     # col=ifelse(diff <  sample$trt, 'blue', 'black'), 
                     col=  ifelse(beta.treatment <  0, ctr$col1 , 
                                  ifelse(beta.treatment >  0, ctr$col2 ,    NA )) ,
                     
                     
                     pch=16
                     , xlab="observed baseline",  ylab="follow up - baseline"  ,
                     
                     main=paste0("Treatment arm: observed responders in blue\nPearson's correlation ",cr),
                     
       cex.main =1.25,
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
          A <- mean(foo < sample$trt)*length(foo)   # 
          AT <- round(A/length(foo)*100,1)
          AN <- length(foo)
          
          T.SENN <- mean(foo < sample$SENN)*length(foo)
          TC.SENN <- round(T.SENN/length(foo)*100,1)
          # ---------------------------------------------------------------------------
          
          trt <- trial[trial$treat==0,]
          trt$diff <- trt$y.1observed - trt$y.0observed
          foo <- sort(trt[,"diff"])
          C <- mean(foo < sample$trt)*length(foo)   # 
          CT <- round(C/length(foo)*100,1)
          CN = length(foo)
          
          C.SENN <-mean(foo < sample$SENN)*length(foo)
          CT.SENN <- round(C.SENN/length(foo)*100,1)
      
      } else { 
        
        
          N <- nrow(trial)
          trt <- trial[trial$treat==1,]
          trt$diff <- trt$y.1observed - trt$y.0observed
          foo <- sort(trt[,"diff"])
          A <- mean(foo > sample$trt)*length(foo)   # 
          AT <- round(A/length(foo)*100,1)
          AN <- length(foo)
          
          
          T.SENN <- mean(foo < sample$SENN)*length(foo)
          TC.SENN <- round(T.SENN/length(foo)*100,1)
         
          # ---------------------------------------------------------------------------
          
          trt <- trial[trial$treat==0,]
          trt$diff <- trt$y.1observed - trt$y.0observed
          foo <- sort(trt[,"diff"])
          C <- mean(foo > sample$trt)*length(foo)   # 
          CT <- round(C/length(foo)*100,1)
          CN = length(foo)
      
          C.SENN <-mean(foo < sample$SENN)*length(foo)
          CT.SENN <- round(C.SENN/length(foo)*100,1)
      }
      
      
      
      Z <- data.frame(AN=AN, A=A, AT=AT, CN=CN, C=C, CT= CT)
      names(Z) <- c("N trt","Observed responders trt",  "%" , "N ctrl","Observed responders ctrl" , "%")
      rownames(Z) <- NULL 
      # ---------------------------------------------------------------------------
      return(list(A=A, AT=AT, C=C, CT= CT, Z=Z, AN=AN, CN=CN, T.SENN=T.SENN, TC.SENN=TC.SENN, C.SENN=C.SENN , CT.SENN=CT.SENN)) 
      
    })
   
    
    
    lmm <- reactive({
      
      sample <- random.sample()
      
      d <- make.data()$d

      
     require(nlme)
    # LMM approach
    m1 <- lme(delta.observed~ treat + y.0observed,
              random=~1|treat , data=d, method="REML",
              weights = varIdent(form = ~1 | treat))
    
    m0 <-lme(delta.observed~ treat + y.0observed,
             random=~1|treat , data=d, method="REML")
    
    # print(m1)
    m2 <- anova(m1,m0) # are the trt ctr interindividual variation in response different?
    # 
    # c.grp <- m1$sigma
    # t.grp <- coef(m1$modelStruct$varStruct, uncons = FALSE)[[1]]*m1$sigma
    # 
    # # true individual response to the intervention estimate
    # sqrt(t.grp^2 - c.grp^2) 
    # 
    # # truth
    # sd(sample()$beta.treatment )
    # 
    return(list(m1=m1, m0=m0, m2=m2)) 
    
    })
    
    
    output$reg.lmm0 <- renderPrint({
      
      return(lmm()$m0)
      
    })
     
    
    output$reg.lmm1 <- renderPrint({
      
      return(lmm()$m1)
      
    })
    
    output$reg.lmm2 <- renderPrint({
      
      return(lmm()$m2)
      
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