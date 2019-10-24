library (shiny)
source ('convMixed.R')
source ('correction_func_improved.R')

correction_ui <- function(id) {
  ns <- NS(id)
  tagList(
  sidebarLayout(
    div(
      style = "width: 70%;",
      sidebarPanel(
        tabPanel(
          "Initial input",
          uiOutput(ns("correctButton")),
          uiOutput(ns('modelSelectedOutput')),
          #uiOutput(ns("ui_formulas_selected")),
          uiOutput(ns("sourceAdjustFor")),
          uiOutput(ns("sourceRemove")),
          uiOutput(ns("sourceShapiro")),
          uiOutput(ns("sourceCorrection")),
          uiOutput(ns("standardDiv")),
          uiOutput(ns("sourceApplyCorrelation")),
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
                withSpinner(DTOutput(ns("resOutput"))), style = "height:'500px'; overflow-y: scroll;overflow-x: scroll;"
              )
            )
          ),
          tabPanel (
            "Selected results",
            box(
              title = "Selected", status = "primary", height =
                "auto", width = "12", solidHeader = T,
              column(
                width = 12,
                withSpinner(DTOutput(ns("selectedOut"))), style = "height:'500px'; overflow-y: scroll;overflow-x: scroll;"
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
                uiOutput(ns('selectTarget')),
                br(),
                withSpinner (DTOutput(ns("correctedListDf"))), style = "height:'500px'; overflow-y: scroll; overflow-x: scroll;" 
              )
            ),
            br (),
            br (),
            downloadButton (ns('downloadSourceCorrected'), 'Download')
          ),
          tabPanel (
            'Plot transformed & untransformed results',
            div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput (ns('selectTarget2'))),
            div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput (ns('selectPredictor'))),
            div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput (ns('selectResponse'))),
            box (
              title = "Model summary for available source types", status = "primary",
              height = "auto", width = "12", solidHeader = T,
              column (
                width = 12,
                DTOutput(ns('modelSummary'))
              )
            ),
            box (
              title = "Plot of transformed & untransformed sources for selected Target", status = "primary",
              height = 870L, width = "12", solidHeader = T,
              column(
                width = 12,
                plotlyOutput(ns('transformedPlot'))
              )
            )
          )
          # ,
          # tabPanel (
          #   'Edit models',
          #   box (
          #     title = "Linear model selected", status = "primary",
          #     height = "auto", width = "12", solidHeader = T,
          #     column (
          #       width = 12,
          #       div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput (ns('selectTarget3'))), #target
          #       div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput (ns('selectSourceType'))), #source
          #       div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput (ns('selectResponse2'))), #response
          #       DTOutput(ns('modelSelected'))
          #     )
          #   ),
          #   box (
          #     title = 'All available models', status = "primary",
          #     height = "auto", width = "12", solidHeader = T,
          #     column (
          #       width = 12,
          #       DTOutput(ns('modelAllAvailable'))
          #     )
          #   )
          # )
        )
      )
    )
  )
 )
}


correction_server <- function(input, output, session, sourceData, targetData){
  
  output$downloadSourceCorrected <- downloadHandler(
    filename = function() {
      paste(paste0("Selected_source-",input$targetID), input, Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(downloadSourceCorrected(), file, row.names = F)
    }
  )
  
  resoutput <- reactiveValues()
  resoutput2 <- reactiveValues(dfList = NULL)
  # radiobutton, select correct or no correct
  output$correctButton <- renderUI({
    ns <- session$ns
    radioButtons(
      ns("correctButton"), "Correct for any of the elements?",
      c(
        "Correct" = "Cor",
        "No correction" = "noCor"
      ),
      selected = "noCor"
    )
  })
  
  # keep only columns in the source that are present in the target
  mySource <- reactive({
    req(sourceData())
    req(targetData())
    sourceData()[, which(names(sourceData()) %in% names(targetData()))]
  })
  
  myTarget <- reactive({
    req (targetData)
    targetData()
  })
  
  # select columns to adjust for
  output$sourceAdjustFor <- renderUI({
    ns <- session$ns
    selectInput(ns("slcCorrect"), "Select columns with size, toc or together", choices = names(mySource())[-c(1, 2)], multiple = TRUE, width = "100%")
  })
  
  # option to pick columns that will not be used for correction
  output$sourceRemove <- renderUI({
    req(input$slcCorrect)
    ns <- session$ns
    if (!is.null(input$slcCorrect)) {
      drops <- c(names(mySource())[c(1, 2)], input$slcCorrect)
    } else {
      drops <- names(mySource())[c(1, 2)]
    }
    selectInput(ns("slcRemove"), "Select columns to remove from correction", choices = names(mySource())[which(!names(mySource()) %in% drops)], multiple = TRUE, width = "100%")
  })
  
  # select p value threshold of normality for residuals
  output$sourceShapiro <- renderUI({
    req(mySource())
    ns <- session$ns
    sliderInput(ns("sourceShapiro"), "Alpha p-value:", value = 0.05, min = 0.001, max = 1, step = 0.01)
  })
  
  output$standardDiv <- renderUI ({
    req(mySource ())
    ns <- session$ns
    sliderInput (ns("sourceStandardDiv"), "Outliers from the number of standard deviates", value = 3, min = 0.1, max = 6, step = 0.1)
  })
  
  # sliderinput for corrplot R threshold
  output$sourceCorrection <- renderUI({
    req(mySource())
    ns <- session$ns
    sliderInput(ns("sourceCorrection"), "Adjusted R2", value = 0.6, min = 0.1, max = 0.99, step = 0.1, animate = F)
  })
  
  # action button, apply
  output$sourceApplyCorrelation <- renderUI({
    req(mySource())
    ns <- session$ns
    actionButton(ns("sourceApplyCorrelation"), "Apply")
  })
  
  dats <- eventReactive(input$sourceApplyCorrelation, {

    if (input$correctButton %in% "Cor"){
      req (input$slcCorrect)
      
      datas <- convert (mySource(), negGlob, zeroConstant)
      resoutput$myresult <- correct.improved(datas,  input$slcCorrect, input$slcRemove, input$sourceStandardDiv, input$sourceCorrection, input$sourceShapiro)
      resoutput$myresultFunc <- resoutput$myresult
      resoutput$myresult
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
  
  # radio button serve as indicator of user choice or default choice
  output$modelSelectedOutput <- renderUI({
    req(mySource())
    ns <- session$ns
    radioButtons(
      ns("modelSelectedInput"), "Select model transformation :",
      c(
        "Best model" = "bestM",
        "Raw model" = "rawM"
      ),
      selected = "bestM"
    )
  })
  
  
  output$resOutput <- renderDT ({
    #req (dats())
    req (selectedOut())
    resoutput$myresult[,4]<- gsub("I(", "(",resoutput$myresult[,4], fixed = T)
    resoutput$myresult
  })
  
  selectedOut <- eventReactive ( input$sourceApplyCorrelation, {
    req (dats())
    req (input$slcCorrect)
    
    
    drops <- NULL
    datas <- convert (mySource(), negGlob, zeroConstant)
    target <- convert (myTarget (), negGlob, zeroConstant)
    
    if (is.null(drops)){
      drops <- cbind ('None', 'None')
      drops <- list(NULL,drops)
    }
    print ('trying correction integrity check')
    corList <- correctionIntegrityCheck(datas, target, resoutput$myresultFunc, input$slcCorrect, drops)
  })
  
  selectedOutTable <- eventReactive (input$sourceApplyCorrelation,{
    selectedOut ()
    target <- convert (myTarget (), negGlob, zeroConstant)
    dat <- selectedCorrectionTabs (selectedOut(), target, input$modelSelectedInput)
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
    selectInput(ns('targetID'), 'Targets', choices = uniqueTargets)
  })
  
  observe ({
    #req (correctedListData())
    resoutput2$dfList <- correctedListData  () #changes to dfList does not trigger reactive action from the
  })
  
  correctedListData <- eventReactive ( input$sourceApplyCorrelation, {
    req (selectedOut())
    target <- convert (myTarget (), negGlob, zeroConstant)
    data <- convert (mySource (), negGlob, zeroConstant)
    dfList <- getListCorrectedData (selectedOut(), target, data, input$slcCorrect,input$modelSelectedInput, negGlob)
    dfList
  })
  
  output$correctedListDf <- renderDT ({
    req (input$targetID)
    i <- which (myTarget()[,1] %in% input$targetID)
    resoutput2$dfList[[i]]
  })
  
  downloadSourceCorrected <- reactive ({
    req (input$targetID)
    i <- which (myTarget()[,1] %in% input$targetID)
    resoutput2$dfList[[i]]
  })
  
  moduleOutput <- reactive ({
    req (input$correctButton)
    drops <- NULL
    if (input$correctButton %in% "Cor"){
      listDataFramesCorrected <- resoutput2$dfList
      if (!is.null(drops)){
        for (i in seq(1, nrow(drops))){
          myindx <- which(as.character(myTarget()[,1]) %in% as.character(drops[i,1]))
          listDataFramesCorrected[[myindx]] <- listDataFramesCorrected[[myindx]][,!(names(listDataFramesCorrected[[myindx]]) %in% drops[i,2])]
        }
      }
      list (listDataFramesCorrected, NULL, NULL)
    } else {

      listDataFrames <- rep(list(mySource()), nrow(myTarget()))
      if (!is.null(drops)){
        for (i in seq(1, nrow(drops))){
          myindx <- which(as.character(myTarget()[,1]) %in% as.character(drops[i,1]))
          listDataFrames[[myindx]] <- listDataFrames[[myindx]][,!(names(listDataFrames[[myindx]]) %in% drops[i,2])]
        }
      }
      list (listDataFrames, NULL, NULL)
    }
  })
  
  #Plot & summary section
  output$selectTarget2 <- renderUI ({
    ns <- session$ns
    trg <- unique(as.character((selectedOutTable())[,1]))
    selectInput(ns('targetSelected'), 'Select Target', choices = trg, selected = trg[1])
  })
  
  output$selectPredictor <- renderUI ({
    ns <- session$ns
    if (length (input$slcCorrect) ==1) {
      selectInput(ns('correctForSelected'), 'Select Predictor', choices = input$slcCorrect, selected = input$slcCorrect)
    } else {
      selectInput(ns('correctForSelected'), 'Select Predictor', choices = input$slcCorrect, selected = input$slcCorrect[1])
    }
  })
  
  output$selectSource <- renderUI ({
    ns <- session$ns
    req (input$correctForSelected)
    req (input$targetSelected)
    subSelected <- selectedOutTable()[which (selectedOutTable()[,1] %in% input$targetSelected),]
    selectInput(ns('sourceTypeSelected'), 'Select Source Type', choices = unique(as.character(subSelected[,2])))
  })
  
  output$selectResponse <- renderUI ({
    ns <- session$ns
    req (input$correctForSelected)
    req (input$targetSelected)
    selectedData <- selectedOutTable()
    subSelected <- selectedData[which (selectedData[,1] %in% input$targetSelected),]
    selectInput(ns('responseSelected'), 'Select Response', choices = unique (as.character(subSelected[,3])), selected = unique (as.character(subSelected[,3]))[1])
  })
  
  #new transformed plot
  output$transformedPlot <- renderPlotly({
    ns <- session$ns
    req (input$targetSelected)
    req (input$responseSelected)
    req (input$correctForSelected)
    
    #x<- inverse_negGlob(x, negGlob, mixed)
    x <- mySource() #untransformed source
    target <- myTarget()
    
    indx <- which (target[,1] %in% input$targetSelected)
    #datas <- correctedListData()[[indx]]
    datas <- resoutput2$dfList[[indx]]
    
    dat <- rbind (datas, x)
    names(dat)[2] <- 'Classes'
    mygrid <- rep ('transformed', nrow (datas))
    mygrid <- c (mygrid, rep ('untransformed', nrow (x)))
    dat <- cbind (dat,mygrid)
    names(dat)[ncol(dat)] <- 'Method'
    suppressMessages(attach(dat))
    suppressWarnings(assign("Response", dat[, c(as.character(input$responseSelected))]))
    suppressWarnings(assign("Predictor", dat[, c(as.character(input$correctForSelected))]))
    p <- ggplot(dat, aes(x = Predictor, y = Response, colour = Method)) +
      geom_point()+
      geom_smooth(method = lm)+
      theme(panel.spacing = unit(2, "lines"), legend.position = "bottom")
    p <- p+facet_wrap(Classes~., ncol = 2, scales = "free") #split in horizontal direction
    ggplotly(p, height = 800, width = 1000) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })
  
  output$modelSummary <- renderDT ({
    req (input$targetSelected)
    req (input$responseSelected)
    req (input$slcCorrect)
    target <- myTarget ()
    selectedData <- selectedOutTable()
    x <- mySource()
    
    indx <- which (target[,1] %in% input$targetSelected)
    subSelected <- selectedData[which (selectedData[,1] %in% input$targetSelected),]
    subSelected <- subSelected[which(subSelected[,3] %in% input$responseSelected),]
    summaryList <- NULL
    for (i in seq (1, nrow(subSelected))){
      subDatas <- x[which (x[,2] %in% subSelected[i,2]),]
      if (length(input$slcCorrect)>1){
        var1 <- subDatas[,input$slcCorrect[1]]
        var2 <- subDatas[,input$slcCorrect[2]]
      } else {
        var1 <- subDatas[,input$slcCorrect]
      }
      conc <- subDatas[,input$responseSelected]
      myfit <- lm(eval(parse(text = subSelected[i,5])))
      mysummary <- tidy(summary(myfit))
      mysummary[,2:ncol(mysummary)] <- apply (mysummary[,2:ncol(mysummary)], 2, round, 5)
      mysummary <- cbind (subSelected[i,2], mysummary)
      summaryList <- rbind(summaryList, data.frame(mysummary))
    }
    summaryList[,2]<- gsub("I(", "(",summaryList[,2], fixed = T)
    names(summaryList)[1] <- 'SourceType'
    summaryList
  })
  
  #Plot & summary section
  output$selectTarget3 <- renderUI ({
    ns <- session$ns
    trg <- unique(as.character((selectedOutTable())[,1]))
    selectInput(ns('targetSelected2'), 'Select Target', choices = trg, selected = trg[1])
  })
  
  #uiSelect Source Types
  output$selectSourceType <- renderUI ({
    ns <- session$ns
    req (input$targetSelected2)
    subSelected <- selectedOutTable()[which (selectedOutTable()[,1] %in% input$targetSelected2),]
    selectInput(ns('sourceTypeSelected2'), 'Select Source Type', choices = unique(as.character(subSelected[,2])))
  })
  
  #ui select responses
  output$selectResponse2 <- renderUI ({
    ns <- session$ns
    req (input$targetSelected2)
    selectedData <- selectedOutTable()
    subSelected <- selectedData[which (selectedData[,1] %in% input$targetSelected2),]
    subSelected <- subSelected [which(subSelected[,2] %in% input$sourceTypeSelected2),]
    selectInput(ns('responseSelected2'), 'Select Response', choices = unique (as.character(subSelected[,3])), selected = unique (as.character(subSelected[,3]))[1])
  })
  
  output$modelSelectedTable <- renderDT ({
    req (input$targetSelected2)
    req (input$sourceTypeSelected2)
    req (input$responseSelected2)
    
    dat <- selectedOutTable ()
    dat <- dat[which(dat[,ncol(dat)]!=TRUE),]
    subdat <- dat[which (dat[,1] %in% input$targetSelected2),]
    subdat <- subdat [which(subdat[,2] %in% input$sourceTypeSelected2),]
    subdat <- subdat [which(subdat[,3] %in% input$responseSelected2),]
    subdat[,5]<- gsub("I(", "(",subdat[,5], fixed = T)
    subdat
  })
  
  modelAllAvailable <- reactive ({
    req (input$targetSelected2)
    req (input$sourceTypeSelected2)
    req (input$responseSelected2)
    
    indx <- which(myTarget ()[,1] %in% input$targetSelected2)
    dat <- selectedOut ()[[indx]]
    dat <- dat[which(dat[,ncol(dat)]!= TRUE),]
    dat <- dat[,-ncol(dat)]
    #subdat <- dat[which (dat[,1] %in% input$targetSelected2),]
    subdat <- dat [which(dat[,1] %in% input$sourceTypeSelected2),]
    subdat <- subdat [which(subdat[,2] %in% input$responseSelected2),]
    subdat[,4]<- gsub("I(", "(",subdat[,4], fixed = T)
    subdat
  })
  
  output$modelAllAvailable <- renderDT ({
    modelAllAvailable ()
  }, selection = 'single')
  
  observeEvent(input$modelAllAvailable_rows_selected, {
    applyformula <-modelAllAvailable()[input$modelAllAvailable_rows_selected,]
    data <- resoutput2$dfList
    target <- input$targetSelected2
    element <- input$responseSelected2
    sourceType <- input$sourceTypeSelected2
    
    targetdf <- myTarget ()
    indx <- which (targetdf[,1] %in% input$targetSelected)
    dat <- data[[indx]]
    subdat <- dat [which(dat[,2] %in% input$sourceTypeSelected2),]
    
    allformulas <- gsub("I(", "(",resoutput$myresult[,4], fixed = T)
    findx <- which (allformulas %in% applyformula[,4])
    useformula <- resoutput$myresultFunc[findx,4]
    
    if (length(input$slcCorrect)>1){
      var1 <- subdat[, which(names(subdat) %in% input$slcCorrect[1])]
      var2 <- subdat[,which(names(subdat) %in% input$slcCorrect[2])]
    } else if (length(input$slcCorrect)==1){
      var1 <- subdat[ ,which(names(subdat) %in% input$slcCorrect)]
    }
    
    x <- mySource()
    x_sub <- x[which(x[,2] %in% input$sourceTypeSelected2),]
    conc <- x_sub[,which(names(x_sub) %in% input$responseSelected2)]
    myfit <- lm(eval(parse(text = useformula)))
    
    targetSelected <- myTarget()[which(myTarget()[,1] %in% input$targetSelected),]
    myreturn <- correctOneVar (x_sub, targetSelected, input$slcCorrect, input$responseSelected2, useformula)
    
    x_sub[,input$responseSelected2] <- myreturn
    resoutput2$dfList[[indx]][which(x[,2] %in% input$sourceTypeSelected2),] <- x_sub
  })
  
  
  return (moduleOutput)
}

#shinyApp(ui = ui, server = server)