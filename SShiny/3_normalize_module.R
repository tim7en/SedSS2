# Normalization and outliers removal
source("normalize_services.R")

normalize_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = ("Shapiro-Wilk test of normality, p values before and after transformations"), status = "success", height = "auto", width = 12, solidHeader = T,
      fluidPage(
        title = "Adjust p-value",
        sidebarLayout(
          div(
            style = "width: 50%;",
            sidebarPanel(
              uiOutput(ns("shapiroP")),
              uiOutput(ns("spPlotpick"))
            ), width = 4
          ),
          mainPanel(
            width = 10,
            column(
              width = 12,
              tabsetPanel(
                tabPanel(
                  "Normality test, Shapiro-Wilk p values",
                  box(
                    title = "Untransformed data p-values", status = "success", height = "auto", width = 12, solidHeader = T,
                    withSpinner(DTOutput(ns("sourceShapiroWilkTable"))),
                    style = "height: 550px; overflow-y: scroll; overflow-x: scroll;"
                  ),
                  br (),
                  br (),
                  downloadButton (ns('downloadShapiroP'), 'Download')
                ),
                navbarMenu(
                  "Transformations",
                  tabPanel(
                    "Methods",
                    box(
                      title = "Methods p-values ", status = "danger", height = "auto", width = 12, solidHeader = T,
                      column(
                        width = 12,
                        withSpinner(DTOutput(ns("getspMethods"))),
                        style = "height:550px; overflow-y: scroll;overflow-x: scroll;"
                      )
                    ),
                    br (),
                    br (),
                    downloadButton (ns('downloadMethods'), 'Download')
                    
                  ),
                  tabPanel(
                    "Achieved p-values",
                    box(
                      title = "Methods p-values ", status = "danger", height = "auto", width = 12, solidHeader = T,
                      column(
                        width = 12,
                        withSpinner(DTOutput(ns("getspPval"))),
                        style = "height: 550px; overflow-y: scroll; overflow-x: scroll;"
                      )
                    ),
                    br (),
                    br (),
                    downloadButton (ns('downloadNewPvalues'), 'Download')
                  )
                ),
                navbarMenu(
                  "QQ-Plots",
                  tabPanel(
                    "Transformed and untransformed plots",
                    box(
                      title = "QQ plot of Original Data", status = "success", height = "auto", width = 6, solidHeader = T,
                      withSpinner(plotlyOutput(ns("getorigQQval"))), style = "height:630px;overflow-y: scroll;overflow-x: scroll;"
                    ),
                    box(
                      title = "QQ plot of Transformed Data", status = "success", height = "auto", width = 6, solidHeader = T,
                      withSpinner(plotlyOutput(ns("getspqqval"))), style = "height:630px;overflow-y: scroll;overflow-x: scroll;"
                    )
                  ),
                  tabPanel(
                    "Only untransformed",
                    box(
                      title = "QQ plot of Original Data", status = "success", height = "auto", width = 12, solidHeader = T,
                      withSpinner(plotlyOutput(ns("getorigQQval2"))), style = "height:630px;overflow-y: scroll;overflow-x: scroll;"
                    )
                  ),
                  tabPanel(
                    "Only transformed",
                    box(
                      title = "QQ plot of Transformed Data", status = "success", height = "auto", width = 12, solidHeader = T,
                      withSpinner(plotlyOutput(ns("getspqqval2"))), style = "height:630px;overflow-y: scroll;overflow-x: scroll;"
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

normalize_server <- function(input, output, session, datinput) {
  
  
  output$downloadShapiroP <- downloadHandler(
    filename = function() {
      paste("Original-p-value", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(computeshapiropval(), file, row.names = F)
    }
  )

  output$downloadMethods <- downloadHandler(
    filename = function() {
      paste("Transform-methods-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(getshapirotransformresult()[[2]], file, row.names = F)
    }
  )
  
  output$downloadNewPvalues <- downloadHandler(
    filename = function() {
      paste("New_p-value", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(getshapirotransformresult()[[1]], file, row.names = F)
    }
  )
  
  datas <- reactive({
    req(datinput())
    req(is.factor(datinput()[, 2]))
    dat <- datinput()
    dat[, 2] <- factor(dat[, 2], levels = c(as.character(unique(dat[, 2]))))
    dat[, 2] <- str_trim(dat[, 2])
    dat[, 2] <- as.factor(dat[, 2])
    return(dat)
  })

  # shapiro-wilk test p value slider
  output$shapiroP <- renderUI({
    ns <- session$ns
    sliderInput(ns("shapiroP"), "Shapiro-Wilk Univariate Normality Test p-value:", value = 0.05, min = 0.001, max = 1, step = 0.01)
  })

  # ui to pick column name for qqplots
  output$spPlotpick <- renderUI({
    req(datinput())
    ns <- session$ns
    selectInput(ns("spPlotpick"), label = "Select element to plot", choices = colnames(datas())[-c(1, 2)])
  })

  # compute shapiro wilk test p value  & output it as data frame
  computeshapiropval <- reactive({
    req(datinput())
    datas <- rawShapiro(datas())
  })

  # output as data frame shapiro wilk table
  output$sourceShapiroWilkTable <- renderDT({
    req(datinput())
    req(is.factor(datas()[, 2]))
    datas <- computeshapiropval()
    if (!is.null(input$shapiroP)) {
      cut <- input$shapiroP
    } else {
      cut <- 0.05
    }
    DT::datatable(datas) %>% formatStyle(
      c(colnames(datas)),
      backgroundColor = styleInterval(c(cut), c("lightsalmon", "white")), fontWeight = "bold"
    )
  })


  getshapirotransformresult <- reactive({
    datID <- datas()[,1]
    data <- datas()[, -1]
    # unique sources
    uniSource <- unique(data[, 1])
    # Final output table
    myDataOutput <- NULL
    myFormulaOutput <- NULL
    myDataMat <- NULL
    
    for (i in uniSource) {
      subsetBySource <- data[which(data[, 1] %in% i), ]
      shapiroPvals <- apply(subsetBySource[, -1], 2, funcTransform, tabversion = "val", pVal = 0.05) # apply each function and check if all the values are numeric
      shapiroPeq <- apply(subsetBySource[, -1], 2, funcTransform, tabversion = "formulas", pVal = 0.05)
      shapiroMat <- apply(subsetBySource[, -1], 2, funcTransform, tabversion = "mat", pVal = 0.05)
      shapiroMat <- cbind (i, shapiroMat)
      myDataOutput <- rbind(myDataOutput, shapiroPvals)
      myFormulaOutput <- rbind(myFormulaOutput, shapiroPeq)
      myDataMat <- rbind (myDataMat, shapiroMat)
    }
    
    # Final output table
    myDataOutput <- as.data.frame(myDataOutput)
    rownames (myDataOutput) <- uniSource
    # Final output table
    myFormulaOutput <- as.data.frame(myFormulaOutput)
    rownames (myFormulaOutput) <- uniSource
    myDataMat <- cbind (as.character(datID), myDataMat)
    myDataMat <- as.data.frame (myDataMat)
    myDataMat[-c(1,2)] <- apply (myDataMat[-c(1,2)], 2, as.numeric)
    colnames (myDataMat)[c(1,2)] <- c('sample', 'SourceType')
    return(list(myDataOutput, myFormulaOutput, myDataMat))
  })

  # output data frame of best methods applied to normalize each class
  output$getspMethods <- renderDT({
    req(is.factor(datas()[, 2]))
    DT::datatable(getshapirotransformresult()[[2]]) %>% formatStyle(
      c(colnames(getshapirotransformresult()[[2]])),
      backgroundColor = styleEqual("None", "lightblue"), fontWeight = "bold"
    )
  })


  # output data frame of best methods applied to normalize each class
  output$getspPval <- renderDT({
    req(datinput())
    req(is.factor(datas()[, 2]))
    if (!is.null(input$shapiroP)) {
      cut <- input$shapiroP
    } else {
      cut <- 0.05
    }
    DT::datatable(getshapirotransformresult()[[1]]) %>% formatStyle(
      c(colnames(getshapirotransformresult()[[1]])),
      backgroundColor = styleInterval(c(cut), c("lightsalmon", "white")), fontWeight = "bold"
    )
  })

  output$getorigQQval <- renderPlotly({
    req(datinput())
    req(is.factor(datas()[, 2]))
    req(input$spPlotpick)
    dat <- datas()
    dat <- dat[order(dat[, 2]), ]
    suppressMessages(attach(dat))
    suppressWarnings(assign("val", get(input$spPlotpick)))
    colnames(dat)[2] <- "Classes"
    p <- ggplot(dat, aes(sample = val, colour = Classes)) +
      stat_qq() + facet_wrap(~Classes, ncol = 2, scales = "free") +
      stat_qq_line() + theme(panel.spacing = unit(2, "lines"), legend.position = "bottom")
    ggplotly(p, height = 400, width = 600) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })

  # plotOutput of shapiro wilk transformations, before and after
  output$getspqqval <- renderPlotly({
    req(datinput())
    req(is.factor(datas()[, 2]))
    methods_dataframe <- data.frame(getshapirotransformresult()[[2]])
    req(getspqqval())
    req(input$spPlotpick)
    datas <- getspqqval()
    dat <- datas
    dat <- dat[order(dat[, 2]), ]
    suppressMessages(attach(dat))
    suppressWarnings(assign("val", get(input$spPlotpick)))
    colnames(dat)[2] <- "Classes"
    plot_annot <- data.frame(as.character(rownames(methods_dataframe)), as.character(methods_dataframe[input$spPlotpick]))
    uniSource <- unique(as.character(rownames(methods_dataframe)))
    uniAnnot <- (as.character(methods_dataframe[input$spPlotpick][, 1]))
    colnames(plot_annot) <- NULL
    plot_dataframe <- cbind(dat[input$spPlotpick], dat$Classes)
    plot_dataframe$annot <- 0

    for (i in seq(1, length(uniSource))) {
      ind <- which(as.character(plot_dataframe[, 2]) == as.character(uniSource[i]))
      plot_dataframe$annot[ind] <- uniAnnot[[i]]
    }
    colnames(plot_dataframe) <- c(input$spPlotpick, "Classes", "Label")
    plot_dataframe[, 2] <- paste(plot_dataframe[, 2], plot_dataframe[, 3], sep = ": ")
    p <- ggplot(plot_dataframe, aes(sample = val, colour = Classes)) +
      stat_qq() + facet_wrap(~Classes, ncol = 2, scales = "free") +
      stat_qq_line() + theme(panel.spacing = unit(2, "lines"), legend.position = "bottom")
    ggplotly(p, height = 400, width = 600) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })

  output$getorigQQval2 <- renderPlotly({
    req(datinput())
    req(is.factor(datas()[, 2]))
    req(input$spPlotpick)
    dat <- datas()
    dat <- dat[order(dat[, 2]), ]
    suppressMessages(attach(dat))
    suppressWarnings(assign("val", get(input$spPlotpick)))
    colnames(dat)[2] <- "Classes"

    p <- ggplot(dat, aes(sample = val, colour = Classes)) +
      stat_qq() + facet_wrap(~Classes, ncol = 2, scales = "free") +
      stat_qq_line() + theme(panel.spacing = unit(2, "lines"), legend.position = "bottom")
    ggplotly(p, height = 800, width = 1000) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })

  # plotOutput of shapiro wilk transformations, before and after
  output$getspqqval2 <- renderPlotly({
    req(datinput())
    req(is.factor(datas()[, 2]))
    methods_dataframe <- data.frame(getshapirotransformresult()[[2]])
    req(getspqqval())
    req(input$spPlotpick)
    datas <- getspqqval()
    dat <- datas
    dat <- dat[order(dat[, 2]), ]
    suppressMessages(attach(dat))
    suppressWarnings(assign("val", get(input$spPlotpick)))
    colnames(dat)[2] <- "Classes"
    plot_annot <- data.frame(as.character(rownames(methods_dataframe)), as.character(methods_dataframe[input$spPlotpick]))
    uniSource <- unique(as.character(rownames(methods_dataframe)))
    uniAnnot <- (as.character(methods_dataframe[input$spPlotpick][, 1]))
    colnames(plot_annot) <- NULL
    plot_dataframe <- cbind(dat[input$spPlotpick], dat$Classes)
    plot_dataframe$annot <- 0

    for (i in seq(1, length(uniSource))) {
      ind <- which(as.character(plot_dataframe[, 2]) == as.character(uniSource[i]))
      plot_dataframe$annot[ind] <- uniAnnot[[i]]
    }
    colnames(plot_dataframe) <- c(input$spPlotpick, "Classes", "Label")
    plot_dataframe[, 2] <- paste(plot_dataframe[, 2], plot_dataframe[, 3], sep = ": ")

    p <- ggplot(plot_dataframe, aes(sample = val, colour = Classes)) +
      stat_qq() + facet_wrap(~Classes, ncol = 2, scales = "free") +
      stat_qq_line() + theme(panel.spacing = unit(2, "lines"), legend.position = "bottom")
    ggplotly(p, height = 800, width = 1000) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })

  # get shapiro-wilk test applied methods
  getspqqval <- reactive({
    req(datinput())
    req(computeshapiropval())
    datas <- getshapirotransformresult()
    datas[[3]]
  })

  return(getspqqval)
}
