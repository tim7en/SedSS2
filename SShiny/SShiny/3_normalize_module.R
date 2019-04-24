# Normalization and outliers removal
source("func.R")

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
                    title = "Untransformed data p-values", status = "success", height = "630", width = 12, solidHeader = T,
                    withSpinner(DTOutput(ns("srcSP"))),
                    style = "height: 550px; overflow-y: scroll; overflow-x: scroll;"
                  )
                ),
                navbarMenu(
                  "Transformations",
                  tabPanel(
                    "Methods",
                    box(
                      title = "Methods p-values ", status = "danger", height = "630", width = 12, solidHeader = T,
                      column(
                        width = 12,
                        withSpinner(DTOutput(ns("getspMethods"))),
                        style = "height:550px; overflow-y: scroll;overflow-x: scroll;"
                      )
                    )
                  ),
                  tabPanel(
                    "Achieved p-values",
                    box(
                      title = "Methods p-values ", status = "danger", height = "630", width = 12, solidHeader = T,
                      column(
                        width = 12,
                        withSpinner(DTOutput(ns("getspPval"))),
                        style = "height: 550px; overflow-y: scroll; overflow-x: scroll;"
                      )
                    )
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
                      withSpinner(plotlyOutput(ns("getspQQval"))), style = "height:630px;overflow-y: scroll;overflow-x: scroll;"
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
                      withSpinner(plotlyOutput(ns("getspQQval2"))), style = "height:630px;overflow-y: scroll;overflow-x: scroll;"
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

  
  datas <- reactive ({
    req (datinput())
    req(is.factor(datinput()[,2]))
    dat <- datinput ()
    dat_glob <<- dat
    dat[,2] <- str_trim (dat[,2])
    dat[,2] <- as.factor(dat[,2])
    return (dat)
  })
  
  # shapiro-wilk test p value slider
  output$shapiroP <- renderUI({
    ns <- session$ns
    sliderInput(ns("shapiroP"), "Shapiro-Wilk Univariate Normality Test p-value:", value = 0.05, min = 0.001, max = 1, step = 0.01)
  })

  # ui to pick column name for qqplots
  output$spPlotpick <- renderUI({
    req (datinput())
    ns <- session$ns
    selectInput(ns("spPlotpick"), label = "Select element to plot", choices = colnames(datas())[-c(1, 2)])
  })

  # compute shapiro wilk test p value  & output it as data frame
  compSP <- reactive({
    req (datinput())
    datas <- rawShapiro(datas())
  })

  # output as data frame shapiro wilk table
  output$srcSP <- renderDT({
    req (datinput())
    req(is.factor(datas()[, 2]))
    datas <- compSP()

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

  # get best transformation methods applied to normalize data
  getspMethods <- reactive({
    req (datinput())
    req(compSP())
    datas <- datas()

    if (!is.null(input$shapiroP)) {
      cut <- input$shapiroP
    } else {
      cut <- 0.05
    }
    res <- transform(datas(), compSP(), cut)[[2]]
  })

  # output data frame of best methods applied to normalize each class
  output$getspMethods <- renderDT({
    req(is.factor(datas()[, 2]))
    DT::datatable(getspMethods()) %>% formatStyle(
      c(colnames(getspMethods())),
      backgroundColor = styleEqual("None", "lightblue"), fontWeight = "bold"
    )
  })

  # get p values of methods applied after shapiro wilk normalization test
  getspPval <- reactive({
    req (datinput())
    req(compSP())

    datas <- datas()

    if (!is.null(input$shapiroP)) {
      cut <- input$shapiroP
    } else {
      cut <- 0.05
    }

    res <- transform(datas, compSP(), cut)[[1]]
  })

  # output data frame of best methods applied to normalize each class
  output$getspPval <- renderDT({
    req (datinput())
    req(is.factor(datas()[, 2]))
    if (!is.null(input$shapiroP)) {
      cut <- input$shapiroP
    } else {
      cut <- 0.05
    }
    DT::datatable(getspPval()) %>% formatStyle(
      c(colnames(getspPval())),
      backgroundColor = styleInterval(c(cut), c("lightsalmon", "white")), fontWeight = "bold"
    )
  })

  output$getorigQQval <- renderPlotly({
    req (datinput())
    req(is.factor(datas()[, 2]))
    req(input$spPlotpick)

    # tryCatch({
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

    # }, warning = function(cond) {}, error = function(cond) {})
  })

  # plotOutput of shapiro wilk transformations, before and after
  output$getspQQval <- renderPlotly({
    req (datinput())
    req(is.factor(datas()[, 2]))
    methods_dataframe <- data.frame(getspMethods())
    # print (getspMethods())
    req(getspQQval())
    req(input$spPlotpick)
    datas <- getspQQval()
    # tryCatch({
    dat <- datas
    dat <- dat[order(dat[, 2]), ]
    suppressMessages(attach(dat))
    suppressWarnings(assign("val", get(input$spPlotpick)))
    colnames(dat)[2] <- "Classes"
    plot_annot <- data.frame(as.character(rownames(methods_dataframe)), as.character(methods_dataframe[input$spPlotpick]))
    uniSource <- unique(as.character(rownames(methods_dataframe)))
    # uniAnnot <- as.character(methods_dataframe[input$spPlotpick])

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
    # print (head(plot_dataframe))

    # print(
    # ggplotly(
    p <- ggplot(plot_dataframe, aes(sample = val, colour = Classes)) +
      stat_qq() + facet_wrap(~Classes, ncol = 2, scales = "free") +
      stat_qq_line() + theme(panel.spacing = unit(2, "lines"), legend.position = "bottom")
    ggplotly(p, height = 400, width = 600) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))

    # }, warning = function(cond) {}, error = function(cond) {})
  })

  output$getorigQQval2 <- renderPlotly({
    req (datinput())
    req(is.factor(datas()[, 2]))
    req(input$spPlotpick)

    # tryCatch({
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

    # }, warning = function(cond) {}, error = function(cond) {})
  })

  # plotOutput of shapiro wilk transformations, before and after
  output$getspQQval2 <- renderPlotly({
    req (datinput())
    req(is.factor(datas()[, 2]))
    methods_dataframe <- data.frame(getspMethods())
    # print (getspMethods())
    req(getspQQval())
    req(input$spPlotpick)
    datas <- getspQQval()
    # tryCatch({
    dat <- datas
    dat <- dat[order(dat[, 2]), ]
    suppressMessages(attach(dat))
    suppressWarnings(assign("val", get(input$spPlotpick)))
    colnames(dat)[2] <- "Classes"
    plot_annot <- data.frame(as.character(rownames(methods_dataframe)), as.character(methods_dataframe[input$spPlotpick]))
    uniSource <- unique(as.character(rownames(methods_dataframe)))
    # uniAnnot <- as.character(methods_dataframe[input$spPlotpick])

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
    # }, warning = function(cond) {}, error = function(cond) {})
  })

  # get shapiro-wilk test applied methods
  getspQQval <- reactive({
    req (datinput())
    req(compSP())

    datas <- datas()

    if (!is.null(input$shapiroP)) {
      cut <- input$shapiroP
    } else {
      cut <- 0.05
    }
    res <- transform(datas, compSP(), cut)
    y <- res[[4]]
    z <- res[[3]]
    datas <- replacevals(datas, y, z)
    datas
  })

  return(getspQQval)
}
