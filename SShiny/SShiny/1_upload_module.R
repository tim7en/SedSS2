# current module used to upload csv file, validate table,
# output corrplot and distribution plots

# functions used outside of main ui module
csvFileInput <- function(id, label = "CSV file") {
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


# Module server function
csvFile <- function(input, output, session, stringsAsFactors) {

  userFile <- reactive({
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  dataframe <- reactive({
    options(warn = -1)
    dats <- read.csv(userFile()$datapath)
    print (class (dats))
    dats <- as.data.frame (dats)
    dats[,3:ncol(dats)] <- apply (dats[,3:ncol(dats)], 2, as.numeric) #convert data frame columns, except 1 and second to numeric
    options(warn = 1)
    dats
  })
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })

  # Return the reactive that yields the data frame
  return(dataframe)
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
                  csvFileInput(ns("file"), "User data (.csv format)"),
                  uiOutput (ns("controls"))
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
                      withSpinner (DTOutput(ns("source_table"))), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
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
                        withSpinner(plotOutput(ns("source_corplot"), width = "100%", height = "600px"))
                      ),
                      column(
                        width = 3,
                        uiOutput(ns("corR")),
                        uiOutput(ns("corMethod")),
                        uiOutput(ns("corType"))
                        # ,
                        # uiOutput(ns("font_size"))
                      )
                    )
                  ),
                  tabPanel(
                    "Distribution",
                    column(
                      width = 12,
                      br(),
                      'Select available columns to plot: ',
                      br (),
                      withSpinner (uiOutput (ns("plotSelect"))),
                      withSpinner(plotOutput(ns("source_distribution"), height = "1000px"))
                    )
                  )
                  ,
                  tabPanel(
                    'Histograms',
                    column (
                      width = 12,
                      br(),
                      'Select available columns to plot: ',
                      br (),
                      withSpinner (uiOutput (ns('source_elements'))),
                      withSpinner(plotlyOutput (ns('histPlot'), height = "800px"))
                    )
                  )
                  #,
                  # tabPanel(
                  #   'Clustering',
                  #   column (
                  #     width = 12,
                  #     br (),
                  #     withSpinner(plotOutput(ns("clusterPlot"), height = "1000px"))
                  #   )
                  # )
                )
              )
            )
          )
        )
      )
    )
  )
}

#server side
upload_server <- function(input, output, session, stringsAsFactors) {
  options(shiny.maxRequestSize = 70 * 1024^2) # max csv data limit set to 60 mb
  input_file <- callModule(csvFile, "file") # nested module
  
  #read in data and output based on column selection
  data_source <- reactive ({
    req (input_file())
    req (input$col)
    indx <- which (input$col %in% colnames(input_file())) #
    if (is.null(indx)) {NULL}
    input_file()[,c(input$col[indx])]
  })
  
  #data table output
  output$source_table <- renderDT({
    data_source()
  })

  #sliderinput for corrplot R threshold
  output$corR <- renderUI({
    req (input$col)
    ns <- session$ns
    sliderInput(ns("corR"), "R", value = 0.6, min = 0, max = 0.99, step = 0.1, animate = F)
  })

  #change corrplot method
  output$corMethod <- renderUI({
    req (input$col)
    ns <- session$ns
    selectInput(ns("corMethod"), "Method", c(
      "shade","pie", "circle", "square", "ellipse", "number", 
      "color"
    ))
  })

  #change corrplot type
  output$corType <- renderUI({
    req (input$col)
    ns <- session$ns
    selectInput(ns("corType"), "Type", c( "lower","full", "upper"))
  })

  #corrplot plot fontsize
  output$font_size <- renderUI({
    req(input$col)
    ns <- session$ns
    sliderInput(ns("font_size"), "FontSize", value = 1, min = 0.1, max = 1.5, step = 0.1)
  })
  
  #selected data columns
  output$controls <- renderUI({ #select input data columns and output to ui
    ns <- session$ns
    req (input_file())
    checkboxGroupInput(ns("col"), "Columns", names(input_file()), selected = names(input_file()))
  })
  
  # corrplot
  output$source_corplot <- renderPlot({
    req (input$corType)
    req (input$col)
    dat <- na.omit(data_source())
    M <- cor(dat[, input$col[-c(1, 2)]])
    M[M < input$corR & M > -input$corR] <- 0 #assign 0 to all values within +- of R2 threshold value
    p <- corrplot(M, method = input$corMethod, order = "hclust", type = input$corType, diag = FALSE)
  })
  
  #selector used to plot specified element
  output$plotSelect <- renderUI ({
    req (data_source ())
    req (input$col)
    ns <- session$ns
    selectInput (ns ('selected_col'), 'Plot', choices = c(input$col), selected = c(sample(input$col, 4)), multiple = TRUE)
  })
  
  # distributions plot
  output$source_distribution <- renderPlot({
    req (data_source())
    req (input$col)
    req (input$selected_col)
    
    if (length(input$col) < 2) {
    } else {
      dat <- na.omit(data_source())
      dat <- dat[, input$selected_col]
      pairs.panels(dat,
                   method = "pearson", # correlation method
                   hist.col = "#00AFBB",
                   density = TRUE, # show density plots
                   ellipses = TRUE # show correlation ellipses
      )
    }
  })
  
  # Cluster plot output
  # output$clusterPlot <- renderPlot ({
  #   req (data_source())
  #   req (input$col)
  #   dat <- na.omit(data_source())
  #   if (length(input$col) < 1){
  # 
  #   } else {
  #     d <- dist(dat[, input$col[-c(1, 2)]], method = "euclidean") # distance matrix
  #     fit <- hclust(d, method="ward.D")
  #     plot (fit)
  #     pvrect(fit, alpha=.95)
  #   }
  # })
  
  #second selector used to plot specified element
  output$source_elements <- renderUI ({
    req (data_source ())
    req (input$col)
    ns <- session$ns
    selectInput (ns ('selected_col2'), 'Plot', choices = c(input$col), selected = c(sample(input$col, 1)))
  })
  
  #histogram plot
  output$histPlot  <- renderPlotly ({
    req (data_source())
    req (input$col)
    req (input$selected_col2)
    
    if (length(input$col) < 2) {
    } else {
      dat <- na.omit(data_source())
      dat <- dat[, input$selected_col2]
      p <- plot_ly(x = dat, type = "histogram")
    }
    
  })
  
  return (data_source)
}
