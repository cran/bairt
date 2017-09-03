#SABIRT
#Shiny App for Bayesian Item Response Theory
#By Javier Martinez and Irene Garcia Mosquera
#
#Maintainer: Javier Martinez <martinezjavier243@gmail.com>
#
#SERVER
#

shiny::shinyServer(function(input, output, session) {

  ####RESET =============================================================
  observeEvent(input$changeData,{

    action <- input[[paste0("goButton", input$changeData - 1, input$changeModel)]]

    if(paste0("initialValues", action, ".RData") %in% list.files()){

      file.remove(paste0("initialValues", action, ".RData"))

    }

    div(
      shinyjs::reset("reset"),
      shinyjs::reset("fileIn"),
      shinyjs::reset("dataTest")
        )

    })#==================================================================


  observeEvent(input$changeModel,{

    action <- input[[paste0("goButton", input$changeData, input$changeModel - 1)]]

    if(paste0("initialValues", action, ".RData") %in% list.files()){

      file.remove(paste0("initialValues", action, ".RData"))

    }

    shinyjs::reset("reset")

  })#====================================================================


  ####OBSERVER ==========================================================
  observe({

    action <- input[[paste0("goButton", input$changeData, input$changeModel)]]

    ##
    if (input$close > 0){

      if( paste0("initialValues", action, ".RData") %in% list.files()){

        file.remove(paste0("initialValues", action, ".RData"))

      }

      stopApp()

    }##

    ##
    if ( is.null(data()) || ncol(data()) < 2 || nrow(data()) < 2 ){

      shinyjs::disable("dataOk")

    }else {

      shinyjs::enable("dataOk")

      }##

    ##
    if(input$model == ""){

      shinyjs::disable(paste0("goButton", input$changeData, input$changeModel))

      shinyjs::disable("iiter")

      shinyjs::disable("burnin")

    }else{

      shinyjs::enable(paste0("goButton", input$changeData, input$changeModel))

      shinyjs::enable("iiter")

      shinyjs::enable("burnin")

    }##

    ##
    if( !is.null( action )){


       if(action > 0){

         shinyjs::disable("model")

       }else{shinyjs::enable("model")}


    if( (action  == 0) ){

      shinyjs::disable("thin")

      }else{

        shinyjs::enable("thin")

      }

    }##

    ##
    if(input$iiter %in% 6:100000 && input$burnin %in% 0:100000
       && input$thin %in% 1:100 && input$model != ""){

      k <- (input$iiter - input$burnin)/(input$thin)

      if( k > 0 && .whole(k) ){

        shinyjs::enable( paste0("goButton", input$changeData, input$changeModel) )

      }else {

        shinyjs::disable( paste0("goButton", input$changeData, input$changeModel) )

      }


      } else {

        shinyjs::disable( paste0("goButton", input$changeData, input$changeModel) )

      }##

    ##
    if(!(input$itemRespon %in% 0:100)){

      shinyjs::disable( paste0("goButton", input$changeData, input$changeModel) )

      }##

    ##
    if(input$dataTest){

      shinyjs::disable("tipo")
      shinyjs::disable("fileIn")
      shinyjs::disable("header")

    }else{

      shinyjs::enable("tipo")
      shinyjs::enable("fileIn")
      shinyjs::enable("header")

      }##


  })#====================================================================


  ####MCMC PANEL ========================================================
  output$mcmcPanel <- renderUI({

    action <- input[[paste0("goButton", input$changeData, input$changeModel)]]

    ####
    if(!is.null(action)){

      ##
      if(input$model == ""){

        return( .modelSelect() )
      }##

      ##
      if(input$model == "2pno" && action == 0){

        return( .mcmc2pnoInfo() )

      }##

      ##
      if(input$model == "3pno" && action == 0){

        return(.mcmc3pnoInfo())

      }##

      ##
      if( action > 0){

      ####
      tabsetPanel(
          #
          tabPanel("Overview and Diagnostic",
            plotOutput( "checkPlot" ),

            fluidRow(
              column(12, .rhat_text())
                    )
          ),#

          #
          tabPanel("Chain",
                   br(), br(),
                   plotOutput( "chainPlot" ),
                   h3( "Chain" ),
                   tags$hr(),

            fluidRow(
              column(5,
                uiOutput("datasetInput"),
                uiOutput("nchainInput")
                ),

              column(7, .chain_text())

                   )
          ),#

          #
          tabPanel("IRC",
                   br(), br(),
                   plotOutput( "IRCPlot" ),
                   h3( "IRC" ),
                   tags$hr(),

            fluidRow(
              column(5,
                numericInput( "ItemIRC", "Item:", 1, min = 1,
                  max = ncol(datamcmc()) ),

                p("Please remember that only exist",
                  code(paste(ncol(datamcmc()), "Items")), ".")
                    ),

              column(7, .irc_text())

                   )
            ),#

          #
          tabPanel("Posterior Densities",
                   br(), br(),
                   plotOutput( "converPlot" ),
                   h3( "Posterior Densities" ),
                   tags$hr(),

            fluidRow(
               column(5,
                      selectInput("ParaPosterior", "Choose a Parameter:",
                                  choices = c("Item Parameters",
                          names(infoParameters()))
                          ),

                      textInput("ItemsPosterior", "Item or ability:", "",
                                width = "50%"),

                      p("Please remember that only exist",
                        code(paste(ncol(datamcmc()), "Items")), "and",
                        code(paste(nrow(datamcmc()), "Examinees")), "."),

                      .ItemsPosterior_text()

                         ),

                     column(7, .parameter_text())

                   )

          ),#

          #
          tabPanel("Summary",
                   br(), br(),
                   dataTableOutput( "contents" ),
                   h3( "Download" ),
                   tags$hr(),

            fluidRow(
              column(5,
                selectInput( "dtipo", "Select Data Type:",
                choices = c(".csv",".csv2",".txt" ) ),
                downloadButton( "downloadData", "Download Table" )),

              column(7, .download_text())

            )

          ),#

          #
          tabPanel("Model Info", br(), br(), uiOutput("modelInfo") )

      )####


      }##

    }#####


  })#====================================================================


  ####CHANGE BUTTON =====================================================
  output$changeButton <- renderUI({

    actionButton( paste0("goButton", input$changeData, input$changeModel) ,
                        "Start MCMC!", style = "color:green" )

  })#====================================================================

  ####DATA ==============================================================
  data <- reactive({

    ##
   if (input$dataTest){

     inFile <- load("www/MathTest.rda")

     inFile <- MathTest

   }else {

     inFile <- input$fileIn$datapath

   }##

    ##
    if ( is.null( inFile ) ){

      NULL

    } else {

     dat <- .readData( inFile, input$tipo, input$header, input$dataTest )

     if(is.null(dat) ){

       NULL

     }else{dat}

    }##

  })#====================================================================

  ####RENDER DATA =======================================================
  output$dataFile <- renderDataTable({

    if(!is.null( data() )){

      data() }else{ NULL }

  })#====================================================================

  ####DATA PANEL ========================================================
  output$dataPanel <- renderUI({

    if(is.null(data()) ){

    return( .modelData() )

    }else{

      return( dataTableOutput( "dataFile" ) )

    }

  })#====================================================================

  #############################MCMC FUNCTION#############################

  ####DATA MCMC =========================================================
  datamcmc <- reactive({

    if(!is.null( data() )){

      as.data.frame( data() )
      #as.data.frame( data()[ , -1 ])

    }else{

      NULL

    }

  })#====================================================================

  ####MCMCLIST ==========================================================
  mcmclist <- reactive({

    action <- input[[paste0("goButton", input$changeData, input$changeModel)]]

    if(!is.null( datamcmc() ) && !is.null(  action )){


      if(action == 0) {out <- NULL} else {

        isolate({

          action

          shinyjs::disable(paste0("goButton", input$changeData, input$changeModel))

          shinyjs::disable("model")

          shinyjs::disable("itemRespon")

          shinyjs::disable("iiter")

          shinyjs::disable("burnin")

          shinyjs::disable("thin")

          shinyjs::disable("changeData")

          shinyjs::disable("changeModel")

          shinyjs::disable("close")

          shinyjs::disable("mcmcPanel")

          shinyjs::disable("downloadData")


          if(action == 1){

            initial <-  NULL

          } else {


            env <-  reactiveFileReader(1000, session,
                                       paste0("initialValues", action - 1, ".RData"),
                                       function( RData, env = new.env() ) {

                                         load(RData, env)

                                         return(env)
                                       })

            initial <-  env()[[names(env())[1]]]

            if ( is.null(initial) ){

              stop( "Error initial.value")

            }

          }


          out <- .mcmc.function( data = datamcmc(), initial.value = initial,
                                 c.prior = prior(),
                                 iter = input$iiter, burning = input$burnin,
                                 thin = input$thin, studyTheta = TRUE,
                                 model = input$model)


          vi <- out$information$final.values

          save( vi, file = paste0("initialValues", action, ".RData"))

          if (action > 1){

            file.remove(paste0("initialValues", action - 1, ".RData"))

          }


          shinyjs::enable(paste0("goButton", input$changeData, input$changeModel))

          shinyjs::enable("itemRespon")

          shinyjs::enable("iiter")

          shinyjs::enable("burnin")

          shinyjs::enable("thin")

          shinyjs::enable("changeData")

          shinyjs::enable("changeModel")

          shinyjs::enable("close")

          shinyjs::enable("mcmcPanel")

          shinyjs::enable("downloadData")

        })


      }

      return( out )

    }else{

      NULL
    }

  })#====================================================================

  ####INFO PARAMETERS===================================================
  infoParameters <- reactive({ table(mcmclist()$diagnostic$diag$Parameter) })

  ####DIAG ==============================================================
  diagn <- reactive({

    diag <- mcmclist()$diagnostic$diag

    if ( is.null( diag ) ){

      return( NULL )

    } else {

      return( diag )

    }

  })#====================================================================

  ####CHECK PLOT ========================================================
  output$checkPlot <- renderPlot({



    if(is.null( mcmclist() )){

      0

    } else {

      bairt::check.plot( mcmclist(), converg.test = TRUE)

    }

  })#====================================================================

  #DIAGNOSTIC TABLE
  output$contents <- renderDataTable({ diagn() })

  ####IRC PLOT ==========================================================
  output$IRCPlot <- renderPlot({


    if( is.null( mcmclist() ) ){

      0

    } else {

      if(input$ItemIRC %in% 1:ncol(datamcmc())){

        bairt::irc( mcmclist(), item = input$ItemIRC,
                    main = paste( "IRC Item", input$ItemIRC ) )

      }else{

        0
      }


    }

  })#====================================================================

  ####PARAMETER =========================================================
  output$converPlot <- renderPlot({

  if( is.null( mcmclist() ) ){

      0

    } else {

    parameter <- input$ParaPosterior

    if (parameter %in% names(infoParameters())){

      items <- .texToNumber(input$ItemsPosterior, infoParameters(), parameter)

      if(!is.null(items) && (items %in% 1:infoParameters()[parameter])){

        bairt::parameter.plot(mcmclist(),
                              items, parameter)
      }

    }else {

      bairt::parameter.plot(mcmclist())

      }


    }

  })#====================================================================

  ####CHAIN PLOT ========================================================
  output$chainPlot <- renderPlot({

    ##
    if( is.null( input$dataset ) || is.null(mcmclist())
        ||  is.null(input$nchain) ){

      0

    } else {

      #
      if (input$dataset != "theta"){

        if( input$nchain %in% 1:ncol(datamcmc()) ){

          bairt::chain.study(mcmclist(), parameter = input$dataset,
                             chain = input$nchain, line = TRUE)

        }else{ 0 }

      }#

      #
      if (input$dataset == "theta"){

        if( input$nchain %in% 1:nrow(datamcmc()) ){

          bairt::chain.study(mcmclist(), parameter = input$dataset,
                             chain = input$nchain, line = TRUE)

        }else{ 0 }

      }#

    }##


  })#====================================================================

  ####MODEL INFO ========================================================
  output$modelInfo <- renderUI({

    if(input$model == "2pno" ){

      return( .mcmc2pnoInfo() )

    }

    if(input$model == "3pno" ){

      return(.mcmc3pnoInfo())

    }

  })#====================================================================

  ####PRIOR =============================================================
  prior <- reactive({

    if( input$itemRespon == 0 ){

      prior <- c( 1, 1) } else {


        alpha <- ( 20 *( 1 / input$itemRespon ) ) + 1

        alpha <- round( alpha, digits = 2 )

        beta <- ( 20 * ( 1 - ( 1 / input$itemRespon ) ) ) + 1

        beta <- round( beta, digits = 2)

        prior <- c( alpha, beta  )


      }

    return( prior )

  })#====================================================================

  ####PRIOR TEXT ========================================================
  output$priorText <- renderUI({


   if (input$itemRespon %in% 0:100){

      return( h5(paste( "c Prior Beta (",
                        prior()[ 1 ], ",", prior()[ 2 ], ")" ) ))

    } else {

      return( h5( "ERROR: Value", style = "color:red" ) )

       }


  })#====================================================================

  ####MCMC TEXT =========================================================
  output$mcmctex <- renderUI({

    k <- (input$iiter - input$burnin)/(input$thin)

    ##
    if(input$iiter %in% 6:100000 && input$burnin %in% 0:100000
       && input$thin %in% 1:100){

       #
       if( k > 0 && .whole(k) ){

          return( h5("OK!") )

      }else{

          if(input$iiter < input$burnin){

             return( h5("ERROR: Iter < Burnin", style = "color:red") )
          }


          if(input$iiter == input$burnin){

            return( h5("ERROR: Iter = Burnin", style = "color:red") )
          }


          if(!(input$iiter == input$burnin) && !(input$iiter < input$burnin)){##############

            return( h5("ERROR: (Iter-Burnin)/Thin is not a whole number",
                       style = "color:red") )
         }

      }#

    } else { h5("ERROR: Value", style = "color:red") }##


 })#=====================================================================

 ####CHAIN DATASET ======================================================
 output$datasetInput <- renderUI({

  selectInput( "dataset", "Choose a Parameter:",
    choices = names(infoParameters()) )

 })#=====================================================================

 ####CHAIN INPUT ========================================================
 output$nchainInput <- renderUI({

  ##
  if(!is.null(input$dataset)){

    #
    if(input$dataset == "theta"){

      nameBUtton <- "Examiner's Ability"

      name <- "Examinees"

      maxValue <- nrow(datamcmc())

    }else{

      nameBUtton <- "Item"

      name <- "Items"

      maxValue <- ncol(datamcmc())

    }#

  }else{

    nameBUtton <- "Item"

    name <- "Items"

    maxValue <- 1

  }##

  div(

    numericInput( "nchain",
                  paste0(nameBUtton, ":"), 1, min = 1, max = maxValue),

    p("Please remember that only exist",
      code(paste(maxValue, name)), ".")
  )

 })#=====================================================================

 ####TEXT GO ============================================================
 output$textGo <- renderUI({

   if(!is.null(mcmclist()) &&
       input[[paste0("goButton", input$changeData, input$changeModel)]] > 0){

     div(
       br(),
       "Press",code("Start MCMC!"), "to continue MCMC."
     )
   } else {""}


 })#====================================================================

 ####DOWNLOAD DATA =====================================================
 output$downloadData <- downloadHandler(

   filename = function() { paste( "Summary-", Sys.Date(),
     .csvf( input$dtipo ), sep = '' ) },

   content = function( file ) {

     .dataWrite( diagn(), input$dtipo,file )

   }

 )#=====================================================================

 ####DATA TEST =========================================================
 output$testData <- renderUI({

   if(is.null(data())){

     div(
       p("No Data")
     )

   }else{

     if(ncol(data()) < 2 || nrow(data()) < 2){

       div( p("ERROR: Data is incorrect"), style = "color:red" )

     }else{

       div(
         p("Please note that:"),
         p(code("Number of Items:"), paste( ncol(data()) ) ),#paste( ncol(data()) - 1 ) ),
         p(code("Number of Examinees:"), paste( nrow(data()) ) )
       )

     }

   }

 })#====================================================================

 ####DATA CONTINUE =====================================================
 output$dataContinue <- renderUI({


   if(is.null(data())){

     div("")

   }else{

     if(ncol(data()) < 2 || nrow(data()) < 2){

       div( "" )

     }else{

       div(
         p("Press",code("Data is Ok!"), "to continue.")
       )

     }

   }

 })#====================================================================

 ####DATA INFO =========================================================
 output$dataInfo <- renderUI({


   if( !is.null(data()) ){

     if(is.null(input$fileIn)){

       name <- "MathTest"

     }else{

       name <- input$fileIn$name
     }

     div(
       column(4, p( strong("Data:"), name)),
       column(4, p( strong("Items:"), paste( ncol(datamcmc()) )  )),
       column(4, p( strong("Examinees:"), paste( nrow(datamcmc()) ) ))
     )

   }else{div("")}

 })#=====================================================================


})

