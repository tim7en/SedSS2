# current module used to upload csv file, validate table,
# output corrplot and distribution plots
# Used for both, source and target in similar fashion
# lower camelcase mode used for UI elements output (input)
# Lower case with underscore used for functions
# All lower case used for reactive functions only


# functions used outside of main ui module
upload_csv <- function(id, label = "CSV file") {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), label,
      multiple = F,
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv"
      )
    )
  )
}


# Module csv input separately
csvfile_module <- function(input, output, session, stringsAsFactors) {
  userfile <- reactive({
    validate(need(input$file, message = FALSE))
    input$file
  })

  inputtable <- reactive({
    options(warn = -1)
    dats <- read.csv(userfile()$datapath)
    print(class(dats))
    dats <- as.data.frame(dats)
    dats[, 3:ncol(dats)] <- apply(dats[, 3:ncol(dats)], 2, as.numeric) # drop first two columns and convert data frame columns to numeric
    options(warn = 1)
    dats
  })

  # Observer to message about uploads
  observe({
    msg <- sprintf("File %s was uploaded", userfile()$name)
    cat(msg, "\n")
  })

  # Return the reactive that yields the data frame
  return(inputtable)
}


# ui side
upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        sidebarLayout(
          div(
            style = "width: 50%;",
            sidebarPanel(
              fluidRow(
                column(
                  width = 12,
                  upload_csv(ns("file"), "User data (.csv format)"),
                  uiOutput(ns("uiControls"))
                )
              ),
              width = 4
            )
          ),
          mainPanel(
            width = 10,
            fluidRow(
              tabsetPanel(
                tabPanel(
                  "Data",
                  box(
                    title = "Input Table", status = "success", height = "630", width = "12", solidHeader = T,
                    column(
                      width = 12,
                      withSpinner(DTOutput(ns("sourceTable"))), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                    )
                  )
                ),
                navbarMenu(
                  "Plots",
                  tabPanel(
                    "Corplot",
                    fluidRow(
                      column(
                        width = 9,
                        br(),
                        withSpinner(plotOutput(ns("sourceCorrplot"), width = "100%", height = "600px"))
                      ),
                      column(
                        width = 3,
                        uiOutput(ns("pearsonRsq")),
                        uiOutput(ns("corMethod")),
                        uiOutput(ns("corType"))
                      )
                    )
                  ),
                  tabPanel(
                    "Distribution",
                    column(
                      width = 12,
                      br(),
                      "Select available columns to plot: ",
                      br(),
                      withSpinner(uiOutput(ns("plotSelect"))),
                      withSpinner(plotOutput(ns("sourceDistribution"), height = "1000px"))
                    )
                  ),
                  tabPanel(
                    "Histograms",
                    column(
                      width = 12,
                      br(),
                      "Select available columns to plot: ",
                      br(),
                      withSpinner(uiOutput(ns("sourceElements"))),
                      withSpinner(plotlyOutput(ns("histPlot"), height = "800px"))
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

# server side
upload_server <- function(input, output, session, stringsAsFactors) {
  options(shiny.maxRequestSize = 70 * 1024^2) # max csv data limit set to 60 mb
  input_file <- callModule(csvfile_module, "file") # nested module

  # read in data and output based on column selection
  datasource <- reactive({
    req(input_file())
    req(input$colNames)
    indx <- which(input$colNames %in% colnames(input_file()))
    if (is.null(indx)) {
      NULL
    }
    input_file()[, c(input$colNames[indx])]
  })

  # data table output
  output$sourceTable <- renderDT({
    datasource()
  })

  # sliderinput for corrplot R threshold
  output$pearsonRsq <- renderUI({
    req(input$colNames)
    ns <- session$ns
    sliderInput(ns("pearsonRsq"), "R", value = 0.6, min = 0, max = 0.99, step = 0.1, animate = F)
  })

  # change corrplot method
  output$corMethod <- renderUI({
    req(input$colNames)
    ns <- session$ns
    selectInput(ns("corMethod"), "Method", c(
      "shade", "pie", "circle", "square", "ellipse", "number",
      "color"
    ))
  })

  # change corrplot type
  output$corType <- renderUI({
    req(input$colNames)
    ns <- session$ns
    selectInput(ns("corType"), "Type", c("lower", "full", "upper"))
  })

  # corrplot plot fontsize
  output$fontSize <- renderUI({
    req(input$colNames)
    ns <- session$ns
    sliderInput(ns("fontSize"), "FontSize", value = 1, min = 0.1, max = 1.5, step = 0.1)
  })

  # selected data columns
  output$uiControls <- renderUI({ # select input data columns and output to ui
    ns <- session$ns
    req(input_file())
    checkboxGroupInput(ns("colNames"), "Columns", names(input_file()), selected = names(input_file()))
  })

  # corrplot, issue with image, we do ignore first two columns, however they are on the sidebar, maybe there should be separate for the elements ??
  output$sourceCorrplot <- renderPlot({
    req(input$corType)
    req(input$colNames)
    dat <- na.omit(datasource())
    M <- cor(dat[, input$colNames[-c(1, 2)]])
    M[M < input$pearsonRsq & M > -input$pearsonRsq] <- 0 # assign 0 to all values within +- of R threshold value
    p <- corrplot(M, method = input$corMethod, order = "hclust", type = input$corType, diag = FALSE)
  })
  

  # selector used to plot specified element
  output$plotSelect <- renderUI({
    req(datasource())
    req(input$colNames)
    ns <- session$ns
    selectInput(ns("selectedColsCorrplot"), "Plot", choices = c(input$colNames[-c(1, 2)]), selected = c(sample(input$colNames, 4)), multiple = TRUE)
  })

  # distributions plot
  output$sourceDistribution <- renderPlot({
    req(datasource())
    req(input$colNames)
    req(input$selectedColsCorrplot)

    if (length(input$colNames) < 2) {
    } else {
      dat <- na.omit(datasource())
      dat <- dat[, input$selectedColsCorrplot]
      pairs.panels(dat,
        method = "pearson", # correlation method
        hist.col = "#00AFBB",
        density = TRUE, # show density plots
        ellipses = TRUE # show correlation ellipses
      )
    }
  })

  # second selector used to plot specified element
  output$sourceElements <- renderUI({
    req(datasource())
    req(input$colNames)
    ns <- session$ns
    selectInput(ns("selectedColsHist"), "Plot", choices = c(input$colNames[-c(1, 2)]), selected = c(sample(input$colNames, 1)))
  })

  # histogram plot
  output$histPlot <- renderPlotly({
    req(datasource())
    req(input$colNames)
    req(input$selectedColsHist)

    if (length(input$colNames) < 2) {
    } else {
      dat <- na.omit(datasource())
      dat <- dat[, input$selectedColsHist]
      p <- plot_ly(x = dat, type = "histogram")
    }
  })

  return(datasource)
}
