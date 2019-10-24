library (shiny)
library (plotly)
library (dplyr)
library (DT)
library (broom)
library (shinydashboard)
library (shinycssloaders)

ui <- fluidPage (
  sidebarLayout(
    sidebarPanel(
    ),
    mainPanel(
      column = 12,
      div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput (ns('selectTarget'))),
      div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput (ns('selectPredictor'))),
      div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput (ns('selectResponse'))),
      box(
        title = "Available options", status = "success", height =
          "auto", solidHeader = T, width = "12",
        column(
          width = 12,
        plotlyOutput(ns('TransformedPlot'))
        )
      ),
      DTOutput(ns('Model_sum'))
    )
  )
)

server <- function (input, output, session){
  
  output$selectTarget <- renderUI ({
    ns <- session$id
    trg <- unique(as.character((selectedData)[,1]))
    selectInput(ns('ui_trg_selected'), 'Select Target', choices = trg, selected = trg[1])
  })
  
  output$selectPredictor <- renderUI ({
    ns <- session$id
    if (length (corFor) ==1) {
      selectInput(ns('ui_corFor_selected'), 'Select Predictor', choices = corFor, selected = corFor)
    } else {
      selectInput(ns('ui_corFor_selected'), 'Select Predictor', choices = corFor, selected = corFor[1])
    }
  })
  
  output$selectSource <- renderUI ({
    ns <- session$id
    req (input$ui_corFor_selected)
    req (input$ui_trg_selected)
    subSelected <- selectedData[which (selectedData[,1] %in% input$ui_trg_selected),]
    selectInput(ns('ui_sourceType_selected'), 'Select Source Type', choices = unique(as.character(subSelected[,2])))
  })
  
  output$selectResponse <- renderUI ({
    ns <- session$id
    req (input$ui_corFor_selected)
    req (input$ui_trg_selected)
    #req (input$ui_sourceType_selected)
    subSelected <- selectedData[which (selectedData[,1] %in% input$ui_trg_selected),]
    #subSelected <- subSelected[which(subSelected[,2] %in% input$ui_sourceType_selected),]
    selectInput(ns('ui_response_selected'), 'Select Response', choices = unique (as.character(subSelected[,3])), selected = unique (as.character(subSelected[,3]))[1])
  })
  
  #new transformed plot
  output$TransformedPlot <- renderPlotly({
    req (input$ui_trg_selected)
    req (input$ui_response_selected)
    req (input$ui_corFor_selected)
    x<- inverse_neg_glob(x, neg_glob, mixed)
    indx <- which (target[,1] %in% input$ui_trg_selected)
    datas <- inverse_neg_glob(correctedDataFrames[[indx]], neg_glob, mixed)
    dat <- rbind (datas, x)
    names(dat)[2] <- 'Classes'
    mygrid <- rep ('transformed', nrow (datas))
    mygrid <- c (mygrid, rep ('untransformed', nrow (x)))
    dat <- cbind (dat,mygrid)
    names(dat)[ncol(dat)] <- 'Method'
    suppressMessages(attach(dat))
    suppressWarnings(assign("Response", dat[, c(as.character(input$ui_response_selected))]))
    suppressWarnings(assign("Predictor", dat[, c(as.character(input$ui_corFor_selected))]))
    p <- ggplot(dat, aes(x = Predictor, y = Response, colour = Method)) +
      geom_point()+
      geom_smooth(method = lm)+
      theme(panel.spacing = unit(2, "lines"), legend.position = "bottom")
    p <- p+facet_wrap(Classes~., ncol = 2, scales = "free") #split in horizontal direction
    ggplotly(p, height = 800, width = 1000) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })
  
  
  output$Model_sum <- renderDT ({
    req (input$ui_trg_selected)
    req (input$ui_response_selected)
    indx <- which (target[,1] %in% input$ui_trg_selected)
    subSelected <- selectedData[which (selectedData[,1] %in% input$ui_trg_selected),]
    subSelected <- subSelected[which(subSelected[,3] %in% input$ui_response_selected),]
    summaryList <- NULL
    for (i in seq (1, nrow(subSelected))){
      subDatas <- x[which (x[,2] %in% subSelected[i,2]),]
      if (length(corFor)>1){
        var1 <- subDatas[,corFor[1]]
        var2 <- subDatas[,corFor[2]]
      }
      conc <- subDatas[,input$ui_response_selected]
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
}
shinyApp(ui, server)