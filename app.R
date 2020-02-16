#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  library(ggplot2)
  library(shiny) 
  library(nlme)
  library(VCA)
  library(MASS)
  library(tidyverse)
  library(shinyWidgets)
  library(shinythemes)  # more funky looking apps
  
  options(max.print=1000000)
  fig.width <- 1375
  fig.height <- 550
  fig.width2 <- 1375  
  fig.height2 <- 730
  fig.width3 <- 800  
  fig.height3 <- 545
  p1 <- function(x) {formatC(x, format="f", digits=1)}
  p2 <- function(x) {formatC(x, format="f", digits=2)}
  options(width=100)
  set.seed(12345) # reproducible
  
  pop=1e6 # this is the population size we take sample from
  is.even <- function(x){ x %% 2 == 0 } # function to id. odd maybe useful
  # Always remember that the purpose of a parallel-group randomized trial is to compare the parallel groups, 
  # NOT to look at change from baseline.  Baseline should always be an adjustment covariate (only).
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(theme = shinytheme("journal"), #https://www.rdocumentation.org/packages/shinythemes/versions/1.1.2
                # paper

                setBackgroundColor(
                  color = c( "#2171B5", "#F7FBFF"),
                  gradient = "linear",
                  direction = "bottom"
                ),

                h2("Responder, non responder fallacy in parallel RCTs"),

                h4("  We perform a simulation of a parallel randomised (1:1) control trial demonstrating one reason why it is wrong to analyse arms 
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
                     #The first slider sets the power and the next alpha level, so we can power the trial as we wish. 
                         h4("
                        
                         The first slider is the 'Treatment effect', every patient in the treatment arm 
                        experiences this effect. So a constant treatment effect is given to ALL in the treated group.
                        Conversely , NO ONE in the control group receives the treatment effect. The 'Random noise' slider is a term representing the within person variation and measurement variation. 
                        The next two sliders are the 'Population mean' and 'Population SD', 
                        this standard deviation is the between 
                        person variation.
                        The next slider imposes, if desired,
                        an inclusion criteria based on the response distribution. All scenarios have 99% power with alpha 1% to pick up the treatment effect."),
                     
                     actionButton(inputId='ab1', label="R code",   icon = icon("th"), 
                                  onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/responder-non-responder-fallacy-in-RCTs/master/app.R', '_blank')"),    
                     actionButton("resample", "Simulate a new sample"),
                     br(), # br(),
                     tags$style(".well {background-color:#b6aebd ;}"), ##ABB0B4AF
                     
                     h4("The first tab 'ANOVA' presents the appropriate approach to estimating the treatment effect for this study design.
                        The next tab presents the observed treatment effect for each patient ordered by magnitude, for each trial arm. There is a typical shape to the distibution,
                        few patients have large changes, most small changes. 
                        The third tab shows the treatment effect by baseline values. Typically we see a negative correlation. 
                        The fourth tab presents plots from the previous two tabs together. 
                        The fifth tab presents an approach to assess the potential for different treatment responses in the trial arm.
                        The sixth tab reproduces via simulation a Stephen Senn example [2].
                        Lastly, a listing of the data is presented."),
                        div(
                            
                          tags$head(
                            tags$style(HTML('#ab1{background-color:orange}'))
                          ),
                          
                          tags$head(
                            tags$style(HTML('#resample{background-color:orange}'))
                          ),
                          
                            #div(strong("Select the parameters using the sliders below"),p(" ")),
                           # div((" ")),
                            #br(),
# 
#                             sliderInput("power", 
#                                         h5("Power"),
#                                         min=.80, max=.99, step=.01, value=.99, 
#                                         ticks=FALSE),
#                             
#                             sliderInput("alpha", 
#                                         h5("Alpha"),
#                                         min=.01, max=.2, step=.01, value=.01, 
#                                         ticks=FALSE),

                            sliderInput("trt",
                                        h5("Treatment effect"),
                                        min=-10, max=10, step=.1, value=-2.5, ticks=FALSE),


                            sliderInput("noise",
                                        h5("Random noise"),
                                        min=0, max=4, step=.2, value=1, ticks=FALSE),

                            
                            sliderInput("pop_mu",
                                        h5("Population mean"),
                                        min=-10, max=10, step=1, value=10, ticks=FALSE),
                            
                            sliderInput("pop_sd",
                                        h5("Population sd"),
                                        min=1, max=10, step=1, value=8, ticks=FALSE),

                            
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
                            tags$a(href = "https://twitter.com/f2harrell/status/1220700181496320001", "[5] Purpose of RCT"),
                             div(p(" ")),
                          tags$a(href = "https://www.nature.com/magazine-assets/d41586-018-07535-2/d41586-018-07535-2.pdf", "[6] Statistical pitfalls of personalized medicine"),
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
                            tabPanel("1 ANCOVA", value=6, 
                                     h4("First we show the preferred approach, estimate the treatment effect with a linear model."),
                                     
                                     fluidRow(
                                       column(width = 5,
                                              div( verbatimTextOutput("reg.summary2")),
                                              p(strong("95% CIs")),
                                              div( verbatimTextOutput("reg.summary3"))
                                       ), 
                                       column(width = 5,
                                              div(plotOutput("ancova.plot", width=fig.width3, height=fig.height3))
                                       )),
                                     h4("Figure 1 ANCOVA model estimating the treatment effect 'treat' whilst adjusting for baseline version of outcome, right panel is a plot of estimated treatment effect, represented by the average vertical distance between the lines. Note the regression lines are from ggplot and not based on the ancova model."),
                            ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end of section to add colour     
                            tabPanel("2 Plot change in order of magnitude", 
                                     div(plotOutput("reg.plot1", width=fig.width, height=fig.height)),  
                                     h4("Figure 2 Observed change in each patient in order of magnitude, blue observed 'responders'. Treated (left) and control arm (right)."),
                                     
                                     h3(" "),
                                     
                                     p(strong("It can be seen that the response value of some treated patients seems to vary at follow-up. 
                                     The same can be said for the patients in the control arm, the response value of some control patients seems to reduce  
                                     whereas for others it seems to change by a small amount or increase at follow-up.")),
                                     
                                     p(strong("In the simulation, the ‘true’ value for all treated patients
                                     changed by a constant value, indicated by the dashed horizontal line (and determined by the 'Treatment effect' slider).
                                     The left panel depicts treated patients only, with observed 'responders' in blue. In truth **EVERYBODY** 
                                     in the treated arm responded to the drug **EQUALLY**! The right panel depicts the control group. Observed responders 
                                     are shown in blue. But in truth **NO ONE** in the control arm responded.")),
                                     
                                     p(strong("Apparent individual difference is due **ENTIRELY** to random within subject error,
                                              measurement error and regression to the mean. Slide the 'Random noise' to zero to see.")),
                                     p(strong("The crux of the problem, focus on the left panel. How do you predict the order of the patients? You cannot, it is random.")),
                                     
                            ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("3 Plot change against baseline",
                                     div(plotOutput("res.plot2", width=fig.width, height=fig.height)),  
                                     h4("Figure 3 Observed individual changes plotted against baseline, treated (left) and control (right) arms."),         
                                     
                                     
                                     p(strong("The negative slope often seen in this type of plot can be due entirely to regression to the mean. Patients with a relatively high measured value
                                     at baseline will naturally regress towards the mean so that follow-up measurements are lower, and vice versa for patients with a relatively low value
                                  at baseline. This regression to the mean leads to the artefact of a negative correlation between change and initial value (or any other variable that is correlated with the initial value).
                                              ")),
                            ),
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("4 All plots together", value=3, 
                                     div(plotOutput("res.plot3", width=fig.width2, height=fig.height2)), 
                                    h4("Figure 4 All plots together. Top, indivduals ordered by increasing observed response in treated (left) and control (right) arms. 
                                        Bottom planels show observed response by baseline in treated (left) and control (right) arms. "),         
                            ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                             tabPanel("5 Analyse the variance!", value=6, 
                                                          h4("Fisher in a letter on this topic in 1938 said to look at the variance in the outcome (suggesting an increase variance in the treated group) [3]. 
                                        Is there any evidence against the null hypothesis? H0: variance is equal in both arms H1: variance is not equal in both arms.
                                        The P-Value testing this hypothesis will for the vast majority of the time not reject the null hypothesis.  
                                        This is what we expect, given that the true magnitude of response in the simulation is constant for all 
                                        patients randomised to the treated arm and constant in the control arm (zero). 
                                        This result provides information that any apparent response differences are negligible 
                                        and any analysis of interindividual response is unwarranted."),
                                     
                                     fluidRow(
                                       column(width = 5,
                                      div( verbatimTextOutput("reg.lmm0")),
                                      div( verbatimTextOutput("reg.lmm1")),
                                    # div( verbatimTextOutput("reg.lmm2")),
                                       ),
                                     
                                     fluidRow(
                                       column(width = 5,
                                             # div( verbatimTextOutput("reg.lmm0")),
                                             # div( verbatimTextOutput("reg.lmm1")),
                                              div( verbatimTextOutput("reg.lmm2")),
                                       ))),
                            ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("6 Clinical relevant difference", 
                                     div(plotOutput("reg.plot4", width=fig.width, height=fig.height)), 
                                     h4("Figure 5 Observed individual changes plotted against baseline, treated (left) and control (right) arms incorporating a clinical relevant difference."),         
                                     
                                     fluidRow(
                                       column(12,
                                              sliderInput("senn",
                                                          strong("Clinical relevant difference"),
                                                          min=-10, max=10, step=.1, value=-2, ticks=FALSE))
                                     ),
                                     #h4("Figure 5 Observed individual changes plotted against baseline, treated (left) and control (right) arms incorporating a clinical relevant difference."),         
                                     
                                     p(strong(" ")),
                                     p(strong("We replicate Stephen Senn's example [2], but using a simulated dataset (one realisation). We can calculate analytically using R
                                              the proportion of treated who will fail to respond by pnorm((-2.5--2)/sqrt(1^2+1^2))= 0.36, see left plot using settings below.
                                              The blue dashed line defines the clinical relevant difference. The black dashed line the constant treatment effect applied 
                                              to EVERYONE in the treated group. Blue circles denote the observed responders.")),
                                     
                                     p(strong("(To replicate Stephen Senn's paper the default values selected are high power and low alpha.
                                              Select 'treatment effect' of -2.5, 'population mean' and 'population SD' do not matter, set 'random noise' to 1, and 
                                              eligibilty to -5, drop this down so that the vast majority of patients are included, finally set the 'Clinical relevant difference' on the above slider to -2.)"
                                               )),
                                  
                                     
                                     h4("Analytical calculation of proportion who will appear to show a
                                     clinically relevant difference in the trial in the treated patients"),
                                     div( verbatimTextOutput("senn.est")),
                                     h4("And in the control patients"),
                                     div( verbatimTextOutput("senn.est2"))
                                     
                            ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("7 Data", value=3, 
                                     DT::dataTableOutput("table1"),
                                     
                            ) 
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        )
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    )
                    
               ) #
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end tab panels 

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
       # SENN <- input$senn
        
        return(list( n=n ,  trt=trt , mu=mu, sd=sd, noise=noise, eligible=eligible, power=power, alpha=alpha))#, SENN =SENN )) 
        
    })
  
    # --------------------------------------------------------------------------
    # ---------------------------------------------------------------------------
    # We had to add code to get around floating point error
    # when noise is zero 
    # # we add integers and then check to see < or > that integer. say it is 1 and 0 being added,
    # but you can get 0 1 and 0.999999999999 which will a problem as I am plotting and coloring if less than 1
    # so we add some to fix this in this section
    # ---------------------------------------------------------------------------
    make.data <- reactive({
        
        sample <- random.sample()
         
        n <-  sample$n                 # this is a v large n which we will take our sample from
        noise <-  sample$noise         # add noise (within person var & meas. error) to the baseline & foll. up
        beta.treatment <-  sample$trt  #  all trt'd subjects exp same trt effect, so no resp - non responders!!
        alpha <- sample$alpha
        power <-   sample$power
        pop_mu <-  sample$mu          # population mean 
        pop_sd <-  sample$sd          # between person SD
        ur.eligible <- sample$eligible # 
        # hard code power rather than user input
        power = .99
        alpha=0.01
        
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
       
        
        return(list(trial=trial,  d=d,  N=N)) 
        
    })

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$summaryx3 <- renderPrint({
      print(make.data()$N)
    }) 

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ANCOVA
    fit <- reactive({
        
      d <- make.data()$d
      f0 <- lm(y.1observed ~ y.0observed + treat, d)
      s <- summary(f0)
      ci <- confint(f0)
      
      return(list(s=s, ci=ci, f0=f0 ))
    })     
    
    
    # https://datascienceplus.com/taking-the-baseline-measurement-into-account-constrained-lda-in-r/
    output$ancova.plot <- renderPlot({         
      
      d <- make.data()$d
      
      d$Group <- ifelse(d$treat== 1, "Treated", "Placebo")

      fav.col=c("#1A425C", "#8AB63F")
      ggplot(d, aes (x=y.0observed,y=y.1observed, col=Group)) + 
        geom_point() + geom_smooth(method="lm", se=FALSE) + 
        scale_color_manual(values=fav.col) + theme_bw() +
        xlab("Baseline version of response") + 
        ylab("Response")
    
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
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # tab 1 plot trt and plot ctrl
    
    output$reg.plot1 <- renderPlot({         
        
        trial <- make.data()$trial
        sample <- random.sample()
        N <- make.data()$N
        stats <- stats()
        beta.treatment <-  sample$trt 
        # for plotting
        diff <- trial$y.1observed - trial$y.0observed
        mi <-  min(diff)*1.2    # for plotting axis
        ma <-  max(diff)*1.2    # for plotting axis
        
         # treated 
         A=stats()$A  #prop
         AT=stats()$AT  #%
         C=stats()$C    
         
         CT=stats()$CT
         AN=stats()$AN   # treated count
         CN=stats()$CN
         # ---------------------------------------------------------------------------
         T.SENN =stats()$T.SENN   # proportion at follow up less than clin relv diff
         C.SENN =stats()$C.SENN
         
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
        abline(h=input$trt, lty=2)
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
        abline(h=input$trt, lty=2)
        title(main = "", sub = "Patients observed to respond coloured blue, otherwise black; dashed horizontal line denotes the true treatment effect, treated only",  
              adj=0,cex.sub = 0.75, font.sub = 1, col.sub = "black"
              
        )
     # ---------------------------------------------------------------------------
        par(mfrow=c(1,1))
    })
    
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # tab 2
    output$res.plot2  <- renderPlot({       
      
      sample <- random.sample()
      beta.treatment <-  sample$trt 
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
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      trt <- trial[trial$treat==1,]
      trt$diff <- trt$y.1observed - trt$y.0observed
      
      cr <- with(trt, cor.test( diff,   y.0observed, method="pearson"))
      cr$estimate[1][[1]]
      cr$conf.int[1:2]
      cr <- paste0( p2(cr$estimate),", 95%CI (",p2(cr$conf.int[1]),", " ,p2(cr$conf.int[2]), " )")
      
      # Due to floating point arithmetic we will see the values will slightly differ and will get a val so setting this to NA
      if (input$noise==0) {  cr <- "NA, 95%CI (NA, NA)"  }
  
      trt$col1 =   ifelse(trt$diff <=  (sample$trt), "blue" , "black")         
      trt$col2 =   ifelse(trt$diff >   (sample$trt), "blue" , "black")           
      
      if ( beta.treatment <  0) {
        trt$colz = trt$col1
        tex <- paste0("Treatment arm: Individual changes against baseline, \nPearson's correlation ",cr,"\n N= ",AN,", No of responders= ",A," (",AT,"%), non responders=",AN-A," (",100-AT,"%)")
      } else {
        trt$colz = trt$col2
        tex <- paste0("Treatment arm: Individual changes against baseline, \nPearson's correlation ",cr,"\n N= ",AN,", No of responders= ",AN-A," (",100-AT,"%), non responders=",A," (",AT,"%)")
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
      with(trt, abline(h=mean(beta.treatment), col=c("forestgreen"), lty="dashed", lwd=c(2) ) )
      
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
      if (input$noise==0) {  cr <- "NA, 95%CI (NA, NA)"  }
      
      ctr$col1 =   ifelse(ctr$diff <=  (sample$trt), "blue" , "black")         
      ctr$col2 =   ifelse(ctr$diff >  (sample$trt), "blue" , "black")   
      
      if ( beta.treatment <  0) {
        ctr$colz = ctr$col1
        tex <- paste0("Control arm: Individual changes against baseline, \nPearson's correlation ",cr,"\n N= ",CN,", No of responders= ",C," (",CT,"%), non responders=",CN-C," (",100-CT,"%)")
      } else {
        ctr$colz = ctr$col2
        tex <- paste0("Control arm: Individual changes against baseline, \nPearson's correlation ",cr,"\n N= ",CN,", No of responders= ",CN-C," (",100-CT,"%), non responders=",CN," (",100-CT,"%)")
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
      
      title(main = "", sub = "Red dashed line is linear regression line of best fit.",  
            adj=0,cex.sub = 0.75, font.sub = 1, col.sub = "black")
      
      par(mfrow=c(1,1))
      
    })
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------   
    # 3rd tab
    output$res.plot3 <- renderPlot({       
      
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
      beta.treatment <-  sample$trt 
      # ---------------------------------------------------------------------------
      par(mfrow=c(2,2))
      # par(bg = 'ivory')
      
      xup <-  max(table(trial$treat))  # new
      trt <- trial[trial$treat==1,]
      trt$diff <- trt$y.1observed - trt$y.0observed
      
      foo <- sort(trt[,"diff"])
      A <- mean(foo < input$trt)*length(foo)   # shown in red
      
      
      foo <- data.frame(foo, col1=NA, col2=NA)
      
      foo$col1 =   ifelse(foo$foo <=    trt$beta.treatment, "blue" , "black")         
      foo$col2 =   ifelse(foo$foo >    trt$beta.treatment, "blue" , "black")   
      
      if (trt$beta.treatment <  0) {foo$colz = foo$col1} else {foo$colz = foo$col2}
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
        tex <- paste0("Control patients, responders coloured blue\n N= ",CN,", No of responders= ",CN-C," (",100-CT,"%), non responders=",C," (",CT,"%)") 
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
      if (input$noise==0) {  cr <- "NA, 95%CI (NA, NA)"  }
      
      trt$col1 =   ifelse(trt$diff <  (sample$trt), "blue" , "black")         
      trt$col2 =   ifelse(trt$diff >  (sample$trt), "blue" , "black")           
      
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
      if (input$noise==0) {  cr <- "NA, 95%CI (NA, NA)"  }
      
      ctr$col1 =   ifelse(ctr$diff <  (sample$trt), "blue" , "black")         
      ctr$col2 =   ifelse(ctr$diff >  (sample$trt), "blue" , "black")   
      
      if ( beta.treatment <  0) {
        ctr$colz = foo$col1
        tex <- paste0("Control arm: Individual changes against baseline, \nPearson's correlation ",cr,"\n N= ",CN,", No of responders= ",C," (",CT,"%), non responders=",CN-C," (",100-CT,"%)")
      } else {
        ctr$colz = foo$col2
        tex <- paste0("Control arm: Individual changes against baseline, \nPearson's correlation ",cr,"\n N= ",CN,", No of responders= ",CN-C," (",100-CT,"%), non responders=",CN," (",100-CT,"%)")
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
      
    })
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # fith tab
    output$reg.plot4 <- renderPlot({         
      
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

      xup <-  max(table(trial$treat))  # new

      trt <- trial[trial$treat==1,]
      trt$diff <- trt$y.1observed - trt$y.0observed

      foo <- sort(trt[,"diff"])

      foo <- data.frame(foo, col1=NA, col2=NA)

      foo$col1 =   ifelse(foo$foo <=    input$senn, "blue" , "black")
      foo$col2 =   ifelse(foo$foo >     input$senn, "blue" , "black")

      if ( input$senn <  0) {
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
      abline(h=input$trt, lty=2)
      abline(h=input$senn, lty=2, col="blue")
      title(main = "", sub = "Patients observed to respond coloured blue, otherwise black; blue dashed line denotes clincal relevant difference",  
            adj=0,cex.sub = 0.75, font.sub = 1, col.sub = "black"
            
      )
      # ---------------------------------------------------------------------------

      trt <- trial[trial$treat==0,]
      trt$diff <- trt$y.1observed - trt$y.0observed
      foo <- sort(trt[,"diff"])

      foo <- data.frame(foo, col1=NA, col2=NA)

      foo$col1 =   ifelse(foo$foo <=     input$senn, "blue" , "black")
      foo$col2 =   ifelse(foo$foo >     input$senn, "blue" , "black")

      if ( input$senn <  0) {foo$colz = foo$col1
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
      abline(h=input$trt, lty=2)
      abline(h=input$senn, lty=2, col="blue")
      title(main = "", sub = "Patients observed to respond coloured blue, otherwise black; black dashed line the true trt effect, which should only manifest in the treated only",  
            adj=0,cex.sub = 0.75, font.sub = 1, col.sub = "black"
            
      )
      
      par(mfrow=c(1,1))
      # ---------------------------------------------------------------------------
    })
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    senn2 <- reactive({
      
      sample <- random.sample()
      
      noise <-  sample$noise        
      beta.treatment <-  sample$trt   
      senn <- (input$senn)
      
      res <- 1- pnorm( (beta.treatment-senn)/ sqrt(noise^2+noise^2)    )
      res2 <- 1-pnorm( (0-senn)/ sqrt(noise^2+noise^2)    )
      return(list(res=res , res2=res2))
    })     
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$senn.est <- renderPrint({
      
      return(senn2()$res)
      
    })
    
    output$senn.est2 <- renderPrint({
      
      return(senn2()$res2)
      
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ---------------------------------------------------------------------------
    # get some counts and percentage for observed resp and non resp
   stats <- reactive({
      
      sample <- random.sample()
      
      trial <- make.data()$trial
      
      
      if (sample$trt < 0) {    
      
          N <- nrow(trial)
      # ---------------------------------------------------------------------------treated
         # trt rel diff -ve
          trt <- trial[trial$treat==1,]
          trt$diff <- trt$delta.observed      # trt effect          
          foo <- sort(trt[,"diff"])                         # sorted treatment effect
          A <- mean(foo <= sample$trt)*length(foo)          # proportion at follow up less than or equal to trt effect
          AT <- round(A/length(foo)*100,1)                  # %
          AN <- length(foo)                                 # count
          
          T.SENN <-   mean(foo < input$senn)*length(foo)   # proportion at follow up less than clin rel diff
          TC.SENN <- round(T.SENN/length(foo)*100,1)        # %
          # ---------------------------------------------------------------------------ctrl
          trt <- trial[trial$treat==0,]                     # same for ctrl
          trt$diff <- trt$delta.observed 
          foo <- sort(trt[,"diff"])
          C <- mean(foo <= sample$trt)*length(foo)   # 
          CT <- round(C/length(foo)*100,1)
          CN = length(foo)
          
          C.SENN <- mean(foo < input$senn)*length(foo)
          CT.SENN <- round(C.SENN/length(foo)*100,1)
      
          
      } else { 
     
           N <- nrow(trial)
           trt <- trial[trial$treat==1,]
           trt$diff <- trt$delta.observed 
           foo <- sort(trt[,"diff"])
           A <- mean(foo > sample$trt)*length(foo)   # 
           AT <- round(A/length(foo)*100,1)
           AN <- length(foo)
           
           T.SENN <- mean(foo < input$senn)*length(foo)
           TC.SENN <- round(T.SENN/length(foo)*100,1)
           # ---------------------------------------------------------------------------
           trt <- trial[trial$treat==0,]
           trt$diff <-trt$delta.observed 
           foo <- sort(trt[,"diff"])
           C <- mean(foo > sample$trt)*length(foo)   # 
           CT <- round(C/length(foo)*100,1)
           CN = length(foo)
           
           C.SENN <-mean(foo < input$senn)*length(foo)
           CT.SENN <- round(C.SENN/length(foo)*100,1)
   }
          
          
          
          
          
          
          
          
          
          
          
          
      # Z <- data.frame(AN=AN, A=A, AT=AT, CN=CN, C=C, CT= CT)
      # names(Z) <- c("N trt","Observed responders trt",  "%" , "N ctrl","Observed responders ctrl" , "%")
      # rownames(Z) <- NULL
      # ---------------------------------------------------------------------------
      return(list(A=A, AT=AT, C=C, CT= CT,  AN=AN, CN=CN, T.SENN=T.SENN, TC.SENN=TC.SENN, #Z=Z,
                  C.SENN=C.SENN , CT.SENN=CT.SENN)) 
      
    })
    # ---------------------------------------------------------------------------
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
    
    # ---------------------------------------------------------------------------
    output$reg.lmm0 <- renderPrint({
      
      return(lmm()$m0)
      
    })
     
    # ---------------------------------------------------------------------------
    
    output$reg.lmm1 <- renderPrint({
      
      return(lmm()$m1)
      
    })
    # ---------------------------------------------------------------------------
    
    output$reg.lmm2 <- renderPrint({
      
      return(lmm()$m2)
      
    })
    # ---------------------------------------------------------------------------
    
   output$A <- renderPrint({
      stats()$A
    }) 
    # ---------------------------------------------------------------------------
    
    output$C <- renderPrint({
      stats()$C
    }) 
    
    # ---------------------------------------------------------------------------
    
    output$xx <- renderPrint({ 
      
      m  <- stats()$Z
      
          return(m )
    })
    
    # ---------------------------------------------------------------------------
    
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