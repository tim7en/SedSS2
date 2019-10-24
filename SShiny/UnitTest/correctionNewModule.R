library (shiny)
library (shinycssloaders)
library (shinydashboard)
library (DT)
source ('convMixed.R')
source ('correction_func_Improved.R')
library(dplyr)
neg_glob <- ('(x+1)/1000, (x*1000)-1')

ui <- fluidPage (
  sidebarLayout(
    div(
      style = "width: 70%;",
      sidebarPanel(
        tabPanel(
          "Initial input",
          uiOutput( ("corBut")),
          uiOutput( ("ui_formulas_selected")),
          uiOutput( ("ui_src_adjustfor")),
          uiOutput( ("ui_src_remove")),
          uiOutput( ("ui_src_shapiro")),
          uiOutput( ("ui_src_cor")),
          uiOutput( ("std")),
          uiOutput( ("ui_src_applycor")),
          br(),
          br()
        )
      )
    ),
    mainPanel(
      width = 9,
      fluidPage(
        tabsetPanel(
          tabPanel(
            "All Options",
            box(
              title = "Available options", status = "success", height =
                "auto", solidHeader = T, width = "12",
              column(
                width = 12,
                withSpinner(DTOutput( ("resOutput"))), style = "height:'500px'; overflow-y: scroll;overflow-x: scroll;"
              )
            )
          ),
          tabPanel (
            "Integrity check",
            box(
              title = "Selected", status = "primary", height =
                "auto", width = "12", solidHeader = T,
              column(
                width = 12,
                withSpinner(DTOutput( ("selectedOut"))), style = "height:'500px'; overflow-y: scroll;overflow-x: scroll;"
              )
            )
          ),
          tabPanel (
            'Corrected data',
            box (
              title = "Correction", status = "primary", height = 
                "auto", width = '12', solidHeader = T,
              column (
                width = 12,
                uiOutput( ('selectTarget')),
                withSpinner (DTOutput(("correctedListDf"))), style = "height:'500px'; overflow-y: scroll; overflow-x: scroll;" 
              )
            )
          ),
          tabPanel (
            'Edit models',
            div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput ( ('selectTarget3'))), #target
            div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput ( ('selectSourceType'))), #source
            div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput ( ('selectResponse2'))), #response
            box (
              title = "Linear model selected",
              column (
                width = 12,
                DTOutput( ('modelSelected')),
                DTOutput( ('modelAllAvailable')),
                actionButton( ('replaceModel'), ' Replace')
              )
            )
          )
        )
      )
    )
  )
)
 

#,
#plot output
#tabOutput
#Tab Output (comparison of multiple models)
#uiOutput ( ('acceptSelection')) 

server<- function (input, output, session, sourceData, targetData){
  
  sourceData <- read.csv("AllenSource.csv")
  targetData <- read.csv ('AllenTarget.csv')
  neg_glob <-('x/1000+1, (x-1)*1000')
  zero_const <- 0.001
  drops <- list('None', cbind('None','None'))
  
  resOutput <- reactiveValues()
  # radiobutton, select correct or no correct
  output$corBut <- renderUI({
    ns <- session$ns
    radioButtons(
      ns("corBut"), "Correct for any of the elements?",
      c(
        "Correct" = "Cor",
        "No correction" = "noCor"
      ),
      selected = "noCor"
    )
  })
  
  #Plot & summary section
  output$selectTarget3 <- renderUI ({
    ns <- session$ns
    trg <- unique(as.character((selectedOutTable())[,1]))
    selectInput( ('ui_trg_selected2'), 'Select Target', choices = trg, selected = trg[1])
  })
  
  #uiSelect Source Types
  output$selectSourceType <- renderUI ({
    ns <- session$ns
    req (input$ui_trg_selected2)
    subSelected <- selectedOutTable()[which (selectedOutTable()[,1] %in% input$ui_trg_selected2),]
    selectInput( ('ui_sourceType_selected2'), 'Select Source Type', choices = unique(as.character(subSelected[,2])))
  })
  
  #ui select responses
  output$selectResponse2 <- renderUI ({
    ns <- session$ns
    req (input$ui_trg_selected2)
    selectedData <- selectedOutTable()
    subSelected <- selectedData[which (selectedData[,1] %in% input$ui_trg_selected2),]
    subSelected <- subSelected [which(subSelected[,2] %in% input$ui_sourceType_selected2),]
    selectInput( ('ui_response_selected2'), 'Select Response', choices = unique (as.character(subSelected[,3])), selected = unique (as.character(subSelected[,3]))[1])
  })
  
  output$modelSelected <- renderDT ({
    req (input$ui_trg_selected2)
    req (input$ui_sourceType_selected2)
    req (input$ui_response_selected2)
    
    dat <- selectedOutTable ()
    dat <- dat[which(dat[,ncol(dat)]!=TRUE),]
    subdat <- dat[which (dat[,1] %in% input$ui_trg_selected2),]
    subdat <- subdat [which(subdat[,2] %in% input$ui_sourceType_selected2),]
    subdat <- subdat [which(subdat[,3] %in% input$ui_response_selected2),]
    subdat[,5]<- gsub("I(", "(",subdat[,5], fixed = T)
    subdat
  })
  
  output$modelAllAvailable <- renderDT ({
    subdat <- modelAllAvailable()
    subdat[,4]<- gsub("I(", "(",subdat[,4], fixed = T)
    subdat
  }, selection = 'single')
  
  modelAllAvailable <- reactive ({
    req (input$ui_trg_selected2)
    req (input$ui_sourceType_selected2)
    req (input$ui_response_selected2)
    
    indx <- which(myTarget ()[,1] %in% input$ui_trg_selected2)
    dat <- selectedOut ()[[indx]]
    dat <- dat[which(dat[,ncol(dat)]!= TRUE),]
    dat <- dat[,-ncol(dat)]
    #subdat <- dat[which (dat[,1] %in% input$ui_trg_selected2),]
    subdat <- dat [which(dat[,1] %in% input$ui_sourceType_selected2),]
    subdat <- subdat [which(subdat[,2] %in% input$ui_response_selected2),]
    subdat
  })

  observeEvent (input$replaceModel,{
    
    # listIndx <- which(myTarget ()[,1] %in% input$ui_trg_selected2)
    # print (resOutput$selectedTabs[[listIndx]])
    # dat <- resOutput$selectedTabs[[listIndx]]
    # indx <- which (dat[,1] %in% input$ui_sourceType_selected2 && dat[,2] %in% input$ui_response_selected2)
    # resOutput$selectedTabs[[listIndx]][indx,] <- modelAllAvailable()[input$modelAllAvailable_rows_selected,]
    # resOutput$selectedTabs[[listIndx]][indx,1] <- input$ui_sourceType_selected2
    # resOutput$selectedTabs[[listIndx]]$OverCorrection[indx] <- 'FALSE'
    # resOutput$selectedTabs[[listIndx]]$rank[indx] <- 1
    # resOutput$selectedTabs
  })
  
  # keep only columns in the source that are present in the target
  mySource <- reactive({
    req(sourceData)
    req(targetData)
    sourceData[, which(names(sourceData) %in% names(targetData))]
  })
  
  myTarget <- reactive({
    req (targetData)
    targetData
  })
  
  # select columns to adjust for
  output$ui_src_adjustfor <- renderUI({
    ns <- session$ns
    selectInput( ("slcCorrect"), "Select columns with size, toc or together", choices = names(mySource())[-c(1, 2)], multiple = TRUE, width = "100%")
  })
  
  # option to pick columns that will not be used for correction
  output$ui_src_remove <- renderUI({
    req(input$slcCorrect)
    ns <- session$ns
    if (!is.null(input$slcCorrect)) {
      drops <- c(names(mySource())[c(1, 2)], input$slcCorrect)
    } else {
      drops <- names(mySource())[c(1, 2)]
    }
    selectInput( ("slcRemove"), "Select columns to remove from correction", choices = names(mySource())[which(!names(mySource()) %in% drops)], multiple = TRUE, width = "100%")
  })
  
  # select p value threshold of normality for residuals
  output$ui_src_shapiro <- renderUI({
    req(mySource())
    ns <- session$ns
    sliderInput( ("ui_src_shapiro"), "Alpha p-value:", value = 0.05, min = 0.001, max = 1, step = 0.01)
  })
  
  output$std <- renderUI ({
    req(mySource ())
    ns <- session$ns
    sliderInput ( ("ui_src_std"), "Outliers from the number of standard deviates", value = 3, min = 0.1, max = 6, step = 0.1)
  })
  
  # sliderinput for corrplot R threshold
  output$ui_src_cor <- renderUI({
    req(mySource())
    ns <- session$ns
    sliderInput( ("ui_src_cor"), "Adjusted R2", value = 0.6, min = 0.1, max = 0.99, step = 0.1, animate = F)
  })
  
  # action button, apply
  output$ui_src_applycor <- renderUI({
    req(mySource())
    ns <- session$ns
    actionButton( ("ui_src_applycor"), "Apply")
  })
  
  dats <- eventReactive(input$ui_src_applycor, {
    if (input$corBut %in% "Cor"){
      datas <- convert (mySource(), neg_glob, zero_const)
      resOutput$myresult <- correct.improved(datas,  input$slcCorrect, input$slcRemove, input$ui_src_std, input$ui_src_cor, input$ui_src_shapiro)
      resOutput$myresultFunc <- resOutput$myresult
    } else {
      
    }
  })
  
  # radio button serve as indicator of user choice or default choice
  output$ui_formulas_selected <- renderUI({
    req(mySource())
    ns <- session$ns
    radioButtons(
      ns("ui_formulas_selected"), "Select data :",
      c(
        "Top rank" = "def",
        "User choice" = "sel"
      ),
      selected = "def"
    )
  })
  
  output$resOutput <- renderDT ({
    req (dats())
    resOutput$myresult[,4]<- gsub("I(", "(",resOutput$myresult[,4], fixed = T)
    resOutput$myresult
  })
  
  #all available correction options
  selectedOut <- reactive ({
    req (dats())
    datas <- convert (mySource(), neg_glob, zero_const)
    target <- convert (myTarget (), neg_glob, zero_const)
    corList <- correctionIntegrityCheck(datas, target, resOutput$myresultFunc, input$slcCorrect, drops)
    corList
  })
  
  #filter table and select one that doesnot overcorrect and with highest pearson correlation
  selectedOutTable <- reactive ({
    selectedOut ()
    target <- convert (myTarget (), neg_glob, zero_const)
    dat <- selectedCorrectionTabs (selectedOut(), target)
    dat
  })
  
  output$selectedOut <- renderDT ({
    req (selectedOut())
    dat <- selectedOutTable()

    dat[,5]<- gsub("I(", "(",dat[,5], fixed = T)
    dat
  })
  
  
  output$selectTarget <- renderUI ({
    target <- myTarget ()
    ns <- session$ns
    uniqueTargets <- unique(as.character(target[,1]))
    selectInput( ('targetID'), 'Targets', choices = uniqueTargets)
  })

  correctedListData <- reactive ({
    req (selectedOut())
    target <- convert (myTarget (), neg_glob, zero_const)
    data <- convert (mySource (), neg_glob, zero_const)
    
    correctionList <- selectedOut()
    resOutput$selectedTabs <- correctionList
    resOutput$selectedTabs

    dfList <- getListCorrectedData (resOutput$selectedTabs, target, data, input$slcCorrect)
  })
  
  output$correctedListDf <- renderDT ({
    req (input$targetID)
    i <- which (myTarget()[,1] %in% input$targetID)
    correctedListData()[[i]]
  })
}

shinyApp(ui = ui, server = server)