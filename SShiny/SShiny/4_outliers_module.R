source ("func.R")

outliers_ui <- function (id){
  ns <- NS(id)
  tagList(
    box(
      title = ("Outliers within standard deviates"), status = "success", height = "auto", width = 12, solidHeader = T,
      fluidPage(
        title = "Adjust number of std",
        sidebarLayout(
          div(
            style = "width: 50%;",
            sidebarPanel(
              uiOutput (ns("nStd")),
              uiOutput (ns("rbSl"))#,
              #uiOutput("dat_text")
            ), width = 4
          ),
          mainPanel(
            width = 10,
            column(
              width = 12,
              tabsetPanel(
                tabPanel(
                  "Data outliers",
                  box(
                    title = "Rows with outliers and selected outliers", status = "success", height =
                      "595", width = "12", solidHeader = T,
                    column(
                      width = 12,
                      tags$hr(),
                      withSpinner (DTOutput(ns("outliersTab1"))), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                    )
                  )
                ),
                navbarMenu(
                  "Review",
                  tabPanel(
                    "Edit selection",
                    box(
                      title = "Outliers ", status = "success", height =
                        "595", width = 12, solidHeader = T,
                      column(
                        width = 12,
                        withSpinner (DTOutput(ns("outliersADtab2"))), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                      )
                    )
                  ),
                  tabPanel(
                    "Standard deviates",
                    box(
                      title = "Standard Normal Deviate", status = "success", height =
                        "595", width = 12, solidHeader = T,
                      column(
                        width = 12,
                        withSpinner (DTOutput (ns("stdTab1"))), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                      )
                    )
                  )
                ),
                tabPanel(
                  "Outliers Removed",
                  box(
                    title = "These Rows Will Be Excluded From Final Output ", status = "danger", height =
                      "595", width = 12, solidHeader = T,
                    column(
                      width = 12,
                      withSpinner (DTOutput(ns("outliersADtab3"))), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                    )
                  )
                ),
                tabPanel(
                  "Final Output",
                  box(
                    title = "Final Output Table", status = "success", height = 
                      "595", width = 12, solidHeader = T,
                    column(
                      width = 12,
                      withSpinner (DTOutput(ns("outliersADtab4"))), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
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

outliers_server <- function (input, output, session, datas, getspQQval) {
  # renderUI number of standard diviates to be considered as an outlier
  output$nStd <- renderUI({
    ns <- session$ns
    sliderInput(ns("nStd"), "Deviates from normal:", value = 2.576, min = 0, max = 6, step = 0.01)
  })
  
  output$rbSl <- renderUI({
    ns <- session$ns
    radioButtons(ns("rbSl"), "User choice", choices = c("Default", "User"), selected = "Default")
  })
  
  # Output transformed data and values with with adjusent asterix if outlier, without order
  shwOutliers <- reactive({
    #req (getspQQval())
    dat <- datas()
    if (!is.null(input$nStd)) {
      cut <- input$nStd
    } else {
      cut <- 2.58
    }
    dfoutput <- detectoutliers(getspQQval(), cut)
    datas <- dfoutput[[1]]
    l <- datas[dfoutput[[2]]]
    datas <- cbind(as.character(dat[, 1]), as.character(dat[, 2]), datas)
    colnames(datas) <- colnames(dat)
    datas <- list(datas, l)
  })
  
  # Output original data and index of outliers, without order
  matchOutliers <- reactive({
    x <- shwOutliers()
    y <- datas()
    output <- x[[1]]
    outliers <- rep("0", dim(output)[1])
    output <- cbind(output, outliers)
    vals <- x[[2]]
    options(warn = -1)
    findRows <- unique((which(is.na(apply(as.data.frame(output[, 3:dim(output)[2]]), 2, as.numeric)), arr.ind = T)[, 1]))
    options(warn = 1)
    output[, ncol(output)][findRows] <- "1"
    output <- as.data.frame(output)
    output <- cbind(y, output$outliers[match(y[, 1], output[, 1])])
    names(output) <- c(colnames(x[[1]]), "outliers")
    # output <- output[order(-(as.numeric(output$outliers))), ]
    transformeddata <- x[[1]]
    transformeddata <- as.data.frame(transformeddata)
    transformeddata <- apply(transformeddata, 2, as.character)
    # transformeddata <- as.matrix(transformeddata)
    vals <- (which(transformeddata %in% x[[2]]))
    output <- list(output, vals)
  })
  
  
  # # compute shapiro wilk test p value  & output it as data frame
  # compSP <- reactive({
  #   datas <- rawShapiro(datas())
  # })
  # 
  # # get shapiro-wilk test applied methods
  # getspQQval <- reactive({
  #   req(datas())
  #   req(compSP())
  #   
  #   datas <- datas()
  #   
  #   # if (!is.null(input$shapiroP)) {
  #   #   cut <- input$shapiroP
  #   # } else {
  #     cut <- 0.05
  #   #}
  #   res <- transform(datas, compSP(), cut)
  #   y <- res[[4]]
  #   z <- res[[3]]
  #   datas <- replacevals(datas, y, z)
  #   datas
  # })
  
  
  
  # DT with outliers found
  outliersTab1 <- reactive({
    vals <- NULL
    datas <- matchOutliers()
    vals <- c(datas[[2]])
    datas <- as.matrix(datas[[1]])
    datas[is.na(datas)] <- "MISSING"
    if (!is.null(matchOutliers()[[2]])) {
      vals <- c(matchOutliers()[[2]])
      datas[as.numeric(vals)] <- paste(datas[as.numeric(vals)], "*", sep = "")
    }
    vals <- c(datas[vals], "MISSING") # Added this (helped to remove dependence!)
    datas <- datas[order(-(as.numeric(datas[, ncol(datas)]))), ]
    datas <- list(datas, vals)
  })
  
  # DT table output for outliers found
  output$outliersTab1 <- renderDT({
    if (is.null(input$outliersADtab2_rows_selected)) {
      datas <- outliersTab1()
      vals <- datas[[2]]
      datas <- (datas [[1]])
    } else {
      datas <- outliersTab1()
      vals <- datas[[2]]
      datas <- (datas [[1]])
      datas[, ncol(datas)] <- 0
      datas[which(datas[, 1] %in% outliersADtab3()[, 1]), ncol(datas)] <- 1
    }
    DT::datatable(datas) %>% formatStyle(
      columns = "outliers",
      target = "row",
      backgroundColor = styleEqual(1, "lightsalmon")
    ) %>% formatStyle(
      columns = colnames(datas),
      backgroundColor = styleEqual(vals, rep("cyan", length(vals)))
    )
  })
  
  # outliers advanced, tab 1
  outliersADtab1 <- reactive({
    datas <- shwOutliers()
    datas2 <- matchOutliers()
    if (length(matchOutliers()[[2]]) == 0) {
      outliersTab1()
    } else {
      vals <- c(datas[[2]])
      datas <- as.matrix(datas[[1]])
      datas[is.na(datas)] <- "MISSING"
      vals <- c(vals, "MISSING")
      datas2 <- as.matrix(datas2[[1]])
      datas <- cbind(datas, datas2[, ncol(datas2)])
      colnames(datas) <- colnames(datas2)
      datas <- datas[order(-(as.numeric(datas[, ncol(datas)]))), ]
      datas <- list(datas, vals)
      datas
    }
  })
  
  # compute data table of standard deviates
  showstd <- reactive({
    req (getspQQval())
    dfoutput <- getSubsetstd(getspQQval())
    outliers <- rep(0, dim(dfoutput)[1])
    output <- cbind(dfoutput, outliers)
    if (!is.null(input$nStd)) {
      cut <- input$nStd
    } else {
      cut <- 2.576
    }
    val <- cut
    output[which(dfoutput > val, arr.ind = T)[, 1], ncol(output)] <- 1
    output[which(dfoutput < -val, arr.ind = T)[, 1], ncol(output)] <- 1
    output <- output[order(-(as.numeric(output[, ncol(output)]))), ]
    
    output
  })
  
  # DT table output for advanced table 1
  output$outliersADtab1 <- renderDT({
    datas <- outliersADtab1()
    vals <- datas[[2]]
    datas <- datas [[1]]
    DT::datatable(datas) %>% formatStyle(
      columns = colnames(datas),
      backgroundColor = styleEqual(vals, rep("darksalmon", length(vals)))
    )
  })
  # output std data tab
  output$stdTab1 <- renderDT({
    if (!is.null(input$nStd)) {
      cut <- input$nStd
    } else {
      cut <- 2.576
    }
    DT::datatable(showstd()) %>% formatStyle(
      columns = c(colnames(showstd())),
      backgroundColor = styleInterval(c(-(cut), cut), c("cyan", "white", "cyan")) # , fontWeight = "bold"
    )
  })
  
  # outliers advanced, tab 2 -> selection and diselection of data rows
  output$outliersADtab2 <- renderDT({
    if (input$rbSl == "Default") {
      datas <- outliersADtab1()
      vals <- datas[[2]]
      datas <- datas [[1]]
      DT::datatable(datas) %>% formatStyle(
        columns = "outliers",
        target = "row",
        backgroundColor = styleEqual(1, "lightsalmon")
      ) %>% formatStyle(
        columns = colnames(datas),
        backgroundColor = styleEqual(vals, rep("cyan", length(vals)))
      )
    } else {
      datas <- outliersADtab1()
      vals <- datas[[2]]
      datas <- datas [[1]]
      print("User choice table")
      DT::datatable(datas) %>% formatStyle(
        columns = colnames(datas),
        backgroundColor = styleEqual(vals, rep("cyan", length(vals)))
      )
    }
  })
  
  # Observe function that will run on NULL values (observing selection of rows for removal)
  an_observe_func <- observe(suspended = T, {
    input$outliersADtab2_rows_selected
    isolate({
      # do stuff here
      # print(input$"outliersADtab2_rows_selected")
    })
  })
  
  # start the observer, without "suspended=T" the observer
  #  will start on init instead of when needed
  an_observe_func$resume()
  
  # outliers advanced, tab 3
  outliersADtab3 <- reactive({
    datas <- outliersADtab1()
    datas <- datas [[1]]
    req (input$rbSl)
    if (input$rbSl == "User") {
      # if (is.null(input$"outliersADtab2_rows_selected")) {
      #   datas <- datas[which(datas[, ncol(datas)] == 1), ]
      # } else {
      if (length(input$outliersADtab2_rows_selected) == 1) {
        datas <- datas[input$outliersADtab2_rows_selected, ]
        t(datas)
      }
      else {
        datas <- datas[input$outliersADtab2_rows_selected, ]
      }
      # }
      data.frame(datas)
    } else {
      datas <- datas[which(datas[, ncol(datas)] == 1), ]
      data.frame(datas)
    }
  })
  
  # outliers advanced, tab 3 Output
  output$outliersADtab3 <- renderDT({
    outliersADtab3()
  })
  
  # outliers advanced, tab 4 Output (final result)
  outliersADtab4 <- reactive({
    datas <- datas()
    datas[which(!datas[, 1] %in% outliersADtab3()[, 1]), ]
  })
  
  output$outliersADtab4 <- renderDT({
    outliersADtab4()
  })
  
  options(warn = 1)
  return(outliersADtab4)
}