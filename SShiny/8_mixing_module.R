# mixing model
source("FunFunc.R")

mixing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        sidebarLayout(
          div(
            style = "width: 50%;",
            sidebarPanel(
              uiOutput(ns("sourceSplitProportion")),
              uiOutput(ns("radioButtonOutput")),
              uiOutput(ns("selectTarget")),
              numericInput(ns("monteCarloSimulations"), "Monte carlo simulations:", 2, min = 1, max = 1000),
              uiOutput(ns("applyMixingModel")),
              br(),
              uiOutput(ns('applySourceVerification'))
            ), width = 4
          ),
          mainPanel(
            width = 10,
            tabsetPanel(
              tabPanel(
                "Table",
                box(
                  title = "Model Output Table", status = "success", height =
                    "auto", solidHeader = T, width = "auto",
                  column(
                    width = 12,
                    withSpinner(DTOutput(ns("runMixingModel"))),
                    style = "height:auto; overflow-y: scroll;overflow-x: scroll;"
                  )
                )
                ,
                box(
                  title = "Model Averages", status = "success", height =
                    "auto", solidHeader = T, width = "auto",
                  column(
                    width = 12,
                    withSpinner(DTOutput(ns("runMixingModelMean"))),
                    style = "height:auto; overflow-y: scroll;overflow-x: scroll;"
                  )
                ),
                br (),
                br (),
                downloadButton (ns('downloadMixingModel'), 'Download')
              ),
              tabPanel(
                "Violin",
                box(
                  title = "Model Plots", status = "success", height =
                    1000, solidHeader = T, width = "auto",
                    uiOutput(ns("targetPlot")),
                    uiOutput(ns("selectViolinPlot")),
                    plotOutput(ns("targetMixingPlot"))
                )
              ),
              tabPanel (
                'Barplot',
                box (
                  title = "Model Plot", status = "success", height = "auto",
                  solidHeader = T, width = "auto",
                  plotOutput (ns('barPlot'))
                )
              ),
              tabPanel(
                "SVT Table",
                box(
                  title = "Model Output Table", status = "success", height =
                    "auto", solidHeader = T, width = "auto",
                  column(
                    width = 12,
                    withSpinner(DTOutput(ns("runSvt"))),
                    style = "height:auto; overflow-y: scroll;overflow-x: scroll;"
                  )
                )
                ,
                br (),
                br (),
                downloadButton (ns('downloadSvt'), 'Download')
              )
            )
          )
        )
      )
    )
  )
}

mixing_server <- function(input, output, session, corrected_function, targetfunction_in, dfaList_x) {
  
  
  output$downloadMixingModel  <- downloadHandler(
      filename = function() {
        paste("Mixing_model_output", input, Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(runmixingmodel(), file, row.names = F)
      }
    )
  
  output$downloadSvt <- downloadHandler(
    filename = function() {
      paste("SVT_output", input, Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(runsourceverification(), file, row.names = F)
    }
  )
  
  
  targetfunction <- reactive({
    dat <- targetfunction_in()
    numvec <- seq(1, nrow(dat))
    dat[, 1] <- paste0("target", numvec)
    dat
  })

  output$applyMixingModel <- renderUI({
    ns <- session$ns
    actionButton(ns("applyMixingModel"), "Run model")
  })
  
  output$applySourceVerification <- renderUI ({
    ns <- session$ns
    actionButton(ns('applySourceVerification'), 'Check SVT')
  })

  output$sourceSplitProportion <- renderUI({
    ns <- session$ns
    sliderInput(ns("sourceSplitProportion"), "Split proportion", value = 0.9, min = 0.1, max = 0.99, step = 0.05, animate = F)
  })

  output$radioButtonOutput <- renderUI({
    ns <- session$ns
    radioButtons(ns("rbMix"), "Select mixing", choices = c("ALL", "SUBSET"), selected = "ALL")
  })

  output$selectTarget <- renderUI({
    req(input$rbMix)
    ns <- session$ns
    if (input$rbMix == "ALL") { } else {
      checkboxGroupInput(ns("selected_targets"), "Targets",
        choices = unique(as.character(targetfunction()[, 1])), selected = NULL, inline = FALSE,
        width = NULL
      )
    }
  })

  output$runMixingModel <- renderDT({
    runmixingmodel()
  })
  
  output$runMixingModelMean <- renderDT ({
    runmixingmodel_glob <<- runmixingmodel ()
    datas <- data.frame(runmixingmodel())
    datas[,-ncol(datas)] <- apply (datas[,-ncol(datas)], 2, as.numeric)
    datas <- datas %>% group_by (target) %>% summarise_all(mean)
    data.frame (datas)
  })

  
  runsourceverification <- eventReactive (input$applySourceVerification, {
    # create cluster
    library(parallel)
    cl <- makeCluster(detectCores() - 1)
    ns <- session$ns
    l <- corrected_function()[[1]] # list of source data
    DFA_l <- dfaList_x() # data frame of dfa
    DFA_l <- DFA_l[, colSums(DFA_l != 0) > 0] #extract only weights over 0
    targetD <- as.data.frame(targetfunction())
    
    for (i in seq (1, length(l))){		
      l[[i]] <- l[[i]][,c(1,2,which (names (l[[i]]) %in% colnames(DFA_l)))]		
    }		
    finalDat <- NULL
    
    if (input$rbMix == "ALL") {
      
    } else if (length(input$selected_targets)>1) {
      l <- l[which(targetfunction()[, 1] %in% input$selected_targets)]
      DFA_l <- DFA_l[which(targetfunction()[, 1] %in% input$selected_targets), ]
    } else {
      l <- l[which(targetfunction()[, 1] %in% input$selected_targets)] #source from the list
      DFA_l <- DFA_l[which(targetfunction()[, 1] %in% input$selected_targets), ]
    }
    modelOutput <- NULL
    
    for (j in seq (1, length (l))){
      targetD <- l[[j]] #source a target
      for (i in seq(1, nrow(targetD))) { # length(l))) {
        target <- targetD[i, -c(1, 2)]
        target <- convert (target, negGlob, zeroConstant)
        DFA <- DFA_l[j, ]
        x <- l[[j]]
        #x <- x[-i,]
        uniSource <- unique(x[, 2])
        uniSource <- as.character(uniSource)
        sourceSplitProportion <- input$sourceSplitProportion
        # use function that was sourced with parallel processing
        parReplicate <- function(cl, n, expr, simplify = TRUE, USE.NAMES = TRUE)
          parSapply(cl, integer(n), function(i, ex) eval(ex, envir = .GlobalEnv),
                    substitute(expr),
                    simplify = simplify, USE.NAMES = USE.NAMES
          )
        clusterExport(cl, list(
          "UseUnMixing", "DFA", "FunFunc", "uniSource", "x", "targetD",
          "sourceSplitProportion", "getSubsetmean", "findMean", "target", "i", "convert", "zeroConstant", "negGlob"
        ), envir = environment())
        output <- parReplicate(cl, 1, FunFunc(), simplify = "matrix")
        output[nrow(output)-1] <- as.character(targetD[i,2])
        output[nrow(output)] <- as.character(targetD[i,1])
        modelOutput <- rbind(modelOutput, t(output))
      }
    }
    stopCluster(cl)
    colnames(modelOutput)[ncol(modelOutput)-1] <- 'Originated'
    modelOutput
  })
  
  output$runSvt <- renderDT({
    runsourceverification ()
  })
  
  
  runmixingmodel <- eventReactive(input$applyMixingModel, {
    # create cluster
    library(parallel)
    cl <- makeCluster(detectCores() - 1)
    ns <- session$ns
    l <- corrected_function()[[1]] # list of source data
    DFA_l <- dfaList_x() # data frame of dfa
    targetD <- as.data.frame(targetfunction())
    DFA_l <- DFA_l[, colSums(DFA_l != 0) > 0]		
    
    for (i in seq (1, length(l))){		
      l[[i]] <- l[[i]][,c(1,2,which (names (l[[i]]) %in% colnames(DFA_l)))]		
    }		
    
    targetD <- targetD[,c(1,2,which (names (targetD) %in% colnames(DFA_l)))]
    finalDat <- NULL

    if (input$rbMix == "ALL") {

    } else if (length(input$selected_targets)>1) {
      l <- l[which(targetfunction()[, 1] %in% input$selected_targets)]
      targetD <- targetD[which(targetfunction()[, 1] %in% input$selected_targets), ]
      DFA_l <- DFA_l[which(targetfunction()[, 1] %in% input$selected_targets), ]
    } else {
      l <- l[which(targetfunction()[, 1] %in% input$selected_targets)] #source from the list
      targetD <- targetD[which(targetfunction()[, 1] %in% input$selected_targets), ]
      DFA_l <- DFA_l[which(targetfunction()[, 1] %in% input$selected_targets), ]
    }
    
    modelOutput <- NULL
    
    if (nrow(targetD) != 0){
      for (i in seq(1, nrow(targetD))) { # length(l))) {
        target <- targetD[i, -c(1, 2)]
        target <- convert (target, negGlob, zeroConstant)
        DFA <- DFA_l[i, ]
        x <- l[[i]]
        uniSource <- unique(x[, 2])
        uniSource <- as.character(uniSource)
        sourceSplitProportion <- input$sourceSplitProportion
        
        # use function that was sourced with parallel processing
        #if (input$monteCarloSimulations > 1) {
        parReplicate <- function(cl, n, expr, simplify = TRUE, USE.NAMES = TRUE)
          parSapply(cl, integer(n), function(i, ex) eval(ex, envir = .GlobalEnv),
            substitute(expr),
            simplify = simplify, USE.NAMES = USE.NAMES
          )
        clusterExport(cl, list(
          "UseUnMixing", "DFA", "FunFunc", "uniSource", "x", "targetD",
          "sourceSplitProportion", "getSubsetmean", "findMean", "target", "i", "convert", "zeroConstant", "negGlob"
        ), envir = environment())
        output <- parReplicate(cl, input$monteCarloSimulations, FunFunc(), simplify = "matrix")
        modelOutput <- rbind(modelOutput, t(output))
      }
    } else {}
    stopCluster(cl)
    modelOutput
  })


  output$targetPlot <- renderUI({
    ns <- session$ns
    req(runmixingmodel())
    dat <- as.data.frame(runmixingmodel())
    un <- unique(as.character(dat$target))
    # selectInput('targetPlot', 'Select target', unique(dat[,ncol(dat)]), selected = NULL)
    selectInput(ns("targetPlot"), "Select target", un, selected = NULL)
  })

  output$targetMixingPlot <- renderPlot({
    req(runmixingmodel())
    req(input$targetPlot)
    dat <- as.data.frame(runmixingmodel())
    dat <- melt(dat, id.vars = c("target"))
    tryCatch({
      req(input$targetPlot)
      req(input$select_violin)
      s <- input$select_violin
      dat <- dat[which(as.character(dat[, 1]) %in% input$targetPlot), ]
      dat <- dat[which(as.character(dat[, 2]) %in% s), ]
      dat$value <- as.numeric(as.character(dat$value))
      ggplot(dat, aes(variable, value, colour = variable)) +
        geom_violin(trim = FALSE) + geom_jitter(height = 0, width = 0.1) +
        geom_boxplot(aes(group = variable), width = 0.1, color = "black", alpha = 0.5) +
        facet_wrap(~variable, ncol = 2, scales = "free")
    }, warning = function(cond) {}, error = function(cond) {})
  }, height = 600, width = 800)

  output$selectViolinPlot <- renderUI({
    ns <- session$ns
    s <- selectedViolinPlots()
    selectInput(ns("select_violin"), "Elements to plot", choices = s, selected = s[-length(s)], multiple = TRUE)
  })

  selectedViolinPlots <- reactive({
    req(runmixingmodel())
    req(input$targetPlot)
    dat <- as.data.frame(runmixingmodel())
    s <- names(dat[, -ncol(dat)])
    s
  })
  
  
  output$barPlot <- renderPlot ({
    req(runmixingmodel())
    dat <- as.data.frame(runmixingmodel())
    dat <- dat[,which (!colnames(dat) %in% ("GOF"))]
    dat <- melt(dat, id.vars = c("target"))
    datas <- dat
    colnames(datas) <- c("Target", "Class", "Proportion")
    datas[, 2] <- as.factor(datas[, 2])
    datas$Proportion <- as.numeric(as.character(datas$Proportion))
    p <- ggplot(datas, aes(x = Target, y = Proportion, fill = Class)) +
      geom_bar(stat = "identity", position = "fill") + ggtitle("Proportion of sediment fluxes")
    p
  })
  
}
