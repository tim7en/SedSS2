source ("func.R")
dfa_ui <- function (id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        sidebarLayout(
          div(
            style = "width: 50%;",
            sidebarPanel(
              uiOutput(ns("rbDFA")),
              uiOutput(ns("ui_dfa_remove")),
              actionButton(ns("applyDFA"), "Apply")
            ), width = 4
          ),
          mainPanel(
            width = 10,
            tabsetPanel(
              tabPanel (
                'DFA data results',
                box(
                  title = "DFA", status = "success", height =
                    "auto", solidHeader = T, width = "auto",
                  column(
                    width = 12,
                    withSpinner(DTOutput(ns("dfa_table"))), style = "height:auto; overflow-y: scroll;overflow-x: scroll;"
                  )
                )
                
              ),
              tabPanel (
                'Bi-plot',
                uiOutput (ns ("dfa_plot_select")),
                withSpinner(plotOutput (ns("dfa_plot")))
                
              ),
              tabPanel (
                '3D-plot',
                box (
                  title = "DFA 3D plot", status = "success", height = 1000, solidHeader = T, width = "auto",
                uiOutput (ns ("dfa_plot_select2")),
                div (withSpinner (plotlyOutput(ns('dfa_plot2'))), align = "center")
                )
              )
            )
          )
        )
      )
    )
  )
}




dfa_server <- function (input, output, session, corrected_function, target_function) {
  
  output$rbDFA <- renderUI({ #buttons for default or user compute
    ns <- session$ns
    radioButtons(
      ns("rbDFA"), "Apply DFA, default or uniform weights?",
      c(
        "Default" = "def",
        "Uniform" = "uni"
      ),
      selected = "def"
    )
  })
  
  
  # option to pick columns that will not be used for correction
  output$ui_dfa_remove <- renderUI({
    ns <- session$ns
    selectInput(ns("dfaR"), "Select columns to remove from DFA", choices = colnames(target_function()[[1]]), multiple = TRUE, width = "100%")
  })
  
  #clean list of source files and remove one from the bracket
  source_l_drop <- reactive ({
    l_src <- corrected_function ()[[1]] #source list data frames
    dat <- target_function()[[1]] # target data
    l_trg <- target_function()[[2]] #target data frame to drop from bracket
    
    #create a data frame of target and drops selected from the input$dfaR
    vecTarget <- rep ("target", nrow (dat))
    for (i in seq (1, length(vecTarget))){
      vecTarget[i] <- paste0(vecTarget[i], i)
    }
    
    d <- NULL
    
    if (!is.null(input$dfaR)){
      for (i in seq (1, length (input$dfaR))){
        for (j in seq (1, length (vecTarget))){
          d <- rbind (d, c(vecTarget[j], input$dfaR[i]))
        }
      }
    }
    
    if (!is.null(d)){
      d <- data.frame (d)
      l_trg <- rbind (l_trg, d)
    }
    
    ##############################

    if (!is.null(l_trg)){
      l_trg[,1] <- gsub ("target", "" ,l_trg[,1])
      for (i in seq (1, length(l_src))){
        lsub <- l_trg[which (as.numeric(l_trg[,1]) == i),]
        dat <- l_src[[i]]
        if (dim(lsub)[1] >0){
          l_src[[i]] <- dat[,!(names(dat) %in% lsub[,2])] #drop these columns from the source data list
        }
      }
      return (l_src) #list of source data
    } else {
      return (l_src) #list of source data
    }
  })
  
  
  dfa_apply_function <- reactive({
    req(corrected_function())
    req (source_l_drop())
    y <- source_l_drop ()
    dat <- stepwiseDFA(y)
    dat #returns list of data frames (dfa)
  })
  
  
  #returns dfa uniform weights
  dfa_default <- reactive ({
    dat <- target_function()[[1]]
    drops <- target_function ()[[2]]
    d <- dim(dat)
    dat_output <- data.frame(matrix(100, nrow = d[1], ncol = (d[2])))
    colnames(dat_output) <- names(dat)
    l_trg <- target_function()[[2]]
    l_trg[,1] <- gsub ("target", "" ,l_trg[,1])
    
    if (!is.null(l_trg)){
      for (i in seq (1, d[1])){
        lsub <- l_trg[which (l_trg[,1] == i),]
        dat_output[i,which (colnames(dat_output) %in% lsub[,2])] <- 0
      }
    }
    
    if (!is.null(input$dfaR)){
      dat_output[, which(colnames(dat_output) %in% input$dfaR)] <- 0
    }
    
    dat_output
  })
  
  
  #returns computed dfa weights
  dfa_compute <- reactive({
    
    dfa_glob <- dfa_apply_function ()
    l_dfa <- list ()
    
    for (i in seq (1, length (dfa_glob))) {l_dfa[[i]] <- dfa_glob[[i]][[1]]}
    
    dat <- target_function()[[1]]
    d <- dim(dat)
    dat_output <- data.frame(matrix(0, nrow = d[1], ncol = (d[2])))
    colnames(dat_output) <- names(dat)
    for (i in seq (1, d[1])){
      dat_output[i, which(colnames(dat_output) %in% l_dfa[[i]][,1])] <- l_dfa[[i]][,3]
    }
    dat_output
  })
  
  output$dfa_plot <- renderPlot ({
    req (dfa_table_function())
    req (input$dfa_number)
    dfa_glob <- dfa_apply_function ()
    l_dfa <- list ()
    for (i in seq (1, length (dfa_glob))) {l_dfa[[i]] <- dfa_glob[[i]][[2]]}
    p <- l_dfa[[as.numeric(input$dfa_number)]]
    p
  },width = 1000, heigh = 600)
  
  output$dfa_plot2 <- renderPlotly ({
    req (dfa_table_function())
    req (input$dfa_number2)
    dfa_glob <- dfa_apply_function ()
    l_dfa <- list ()
    for (i in seq (1, length (dfa_glob))) {l_dfa[[i]] <- dfa_glob[[i]][[3]]}
    p <- l_dfa[[as.numeric(input$dfa_number2)]]
    p
  })
  
  output$dfa_plot_select <- renderUI({
    ns <- session$ns
    req (dfa_table_function())
    req (dfa_compute())
    req (dfa_apply_function ())
    s <- seq (1, length (dfa_apply_function()))
    selectInput(ns ('dfa_number'), 'Select source', selected = s[1], choices = s )
  })
  
  output$dfa_plot_select2 <- renderUI({
    ns <- session$ns
    req (dfa_table_function())
    req (dfa_compute())
    req (dfa_apply_function ())
    s <- seq (1, length (dfa_apply_function()))
    selectInput(ns ('dfa_number2'), 'Select source', selected = s[1], choices = s )
  })
  
  
  dfa_table_function <- eventReactive(input$applyDFA, (
    if (input$rbDFA == "def") {
      dat <- dfa_compute ()
      dat
    } else {
      dat <- dfa_default ()
      dat
    }
  ))
  
  #output data table
  output$dfa_table <- renderDT ({
    dfa_table_function ()
  })
  
  return (dfa_table_function)
  
}