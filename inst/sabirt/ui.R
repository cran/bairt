#SABIRT
#Shiny App for Bayesian Item Response Theory
#By Javier Martinez and Irene Garcia Mosquera
#
#Maintainer: Javier Martinez <martinezjavier243@gmail.com>
#
#UI
#

source("info.R")

source("utility.sabirt.R")

source("utility.test.R")

source("utility.2pnob.R")

source("utility.3pnob.R")


shiny::shinyUI(fluidPage(

  shinyjs::useShinyjs(),
  shiny::withMathJax(),

  ###INTRO ================================================================
  fluidRow(
    column(12,

     #WELCOME ____________________________________________
     conditionalPanel(condition= "input.welcomeOk == 0",

     fluidRow(
       column(3,""),

       #INFO _____________________________________________
       column(6,
         fluidRow(  .intro()   ),
           br(),br(),br(),br(),

           fluidRow(

             column(4, ""),

             column(4,

               actionButton( "welcomeOk", "Start!", style = "color:blue" )

                ),

           column(4, "")

           )

       ),#________________________________________________

       column(3,"")

       ))#________________________________________________

      )),#================================================================


  ###INFO ================================================================
  conditionalPanel("input.welcomeOk == 1",

   #TITLE ________________________________________________
  fluidRow(
    column(10,
      img(src = "SABIRT22.png", height = 150, width = 350),
      #div( titlePanel("SABIRT"), style = "color:blue"),

      div(#h4("Shiny App for Bayesian Item Response Theory" ),
          h5("by Javier Martinez and Irene Garcia Mosquera"),
          h6("Maintainer: Javier Martinez <martinezjavier243@gmail.com>")
      )

       ),

    #VIEW TIME ____________________________________________
    column(2,

      br(),br(),
      p( "", span(id = "time", date() ) )
       )

    ),#____________________________________________________

   br(),# br(),

  #BUTTONS ________________________________________________
  fluidRow(
    column(6,

    conditionalPanel(
      condition = "input.dataOk != input.changeData",
         uiOutput("dataInfo")
                    )
           ),

    column(6,

       column(3, ""),

       column(3,

         conditionalPanel(
           condition = "input.model != ''",
              actionButton("changeModel", "Change Model!", style = "color:blue")
                         )
              ),

       column(3,

         conditionalPanel(
           condition = "input.dataOk != input.changeData",
              actionButton("changeData", "Change Data!", style = "color:blue")
                          )
           ),

       column(3,

             actionButton("close", "Close App!", style = "color:red",
                          onclick = "setTimeout(function(){window.close();},500);")
              )

             )

  ),#_______________________________________________________

  #REMARK ________________________________________________
  fluidRow(

    column(9,""),

    column(3,
           br(),

           p("Use", code("Close App!"),  "button", "for", code("CLOSE SABIRT"),
             ". If you don't it, you will can create fails."))

  )#______________________________________________________


  ),#=====================================================================


  ####DATA PANEL =========================================================
  conditionalPanel(
    condition= "(input.welcomeOk == 1) && (input.dataOk == input.changeData)",

    fluidRow(

      #LOAD DATE __________________________________________
      column(3,

        h3( "Data Panel" ),

        tags$hr(),

        checkboxInput( "dataTest", "MathTest Data", FALSE ),

        selectInput( "tipo", "Type of File Data:",
                     choices = c(".csv",".csv2",".txt" ) ),
        fileInput( "fileIn", "Choose a File",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv",".xlsx",".tex" ) ),

        checkboxInput("header", "Header", TRUE),

        uiOutput("testData"),

        br(),

          actionButton( "dataOk", "Data is Ok!", style = "color:green" ),

        br(),br(),

          uiOutput("dataContinue")


      ),#__________________________________________________


      #DATA _______________________________________________
      column(9,

        h3( "Data" ),

        tags$hr(),

        uiOutput("dataPanel")

      )#___________________________________________________

    )

    ),#===================================================================

  br(),

  ########
  div(id = "reset",

  ####MODEL PANEL ========================================================
  conditionalPanel(
    condition = "input.dataOk != input.changeData && input.welcomeOk == 1",

    fluidRow(
        #MODEL PANEL ______________________________________
        column(3,

         h3("Model Panel"),

         tags$hr(),

         selectInput( "model", "Choose a Model:",
                      choices = c("", "2pno","3pno" ) ),

         conditionalPanel(condition = "input.model == '3pno'",

            numericInput( "itemRespon", "Number of Response Options:",
                         0, min = 0, max = 100 ),

            uiOutput( "priorText" )

                             ),

          numericInput( "iiter", "Iteration:", 3000, min = 6, max = 100000 ),

          numericInput( "burnin", "Burn-in:", 0, min = 1, max = 100000),

          numericInput( "thin", "Thinin:", 1, min = 1, max = 100),

          conditionalPanel( condition = "input.model != ''",

            uiOutput("mcmctex")

            ),

          uiOutput("changeButton"),

          uiOutput("textGo")

              ),#__________________________________________

        #MODEL INFO _________________________
        column(9,

        h3( "MCMC Panel" ),

        tags$hr(),

        uiOutput( "mcmcPanel" )
        )#___________________________________

    ))#===================================================================


  ),########

  br(),br()

))
