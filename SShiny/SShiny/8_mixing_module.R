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
              uiOutput(ns("ui_src_applymix")),
              uiOutput(ns("ui_src_split")),
              uiOutput(ns("radioBut")),
              uiOutput(ns("selectTarget")),
              numericInput(ns("mcsimulations"), "Monte carlo simulations:", 2, min = 1, max = 1000)
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
                    withSpinner(DTOutput(ns("mixingOutput"))),
                    style = "height:auto; overflow-y: scroll;overflow-x: scroll;"
                  )
                )
              ),
              tabPanel(
                "Violin",
                box(
                  title = "Model Plots", status = "success", height =
                    1000, solidHeader = T, width = "auto",
                    uiOutput(ns("targetPlot")),
                    uiOutput(ns("select_violin_plot")),
                    plotOutput(ns("trg_mixing_plot"))
                )
              ),
              tabPanel (
                'Barplot',
                box (
                  title = "Model Plot", status = "success", height = "auto",
                  solidHeader = T, width = "auto",
                  plotOutput (ns('mybarplot'))
                )
              )
            )
          )
        )
      )
    )
  )
}

mixing_server <- function(input, output, session, corrected_function, target_function_in, dfaList_x) {
  target_function <- reactive({
    dat <- target_function_in()
    numvec <- seq(1, nrow(dat))
    dat[, 1] <- paste0("target", numvec)
    dat
  })

  output$ui_src_applymix <- renderUI({
    ns <- session$ns
    actionButton(ns("ui_src_applymix"), "Use mixing")
  })

  output$ui_src_split <- renderUI({
    ns <- session$ns
    sliderInput(ns("ui_src_split"), "Split proportion", value = 0.9, min = 0.1, max = 0.99, step = 0.05, animate = F)
  })

  output$radioBut <- renderUI({
    ns <- session$ns
    radioButtons(ns("rbMix"), "Select mixing", choices = c("ALL", "SUBSET"), selected = "ALL")
  })

  output$selectTarget <- renderUI({
    req(input$rbMix)
    ns <- session$ns
    if (input$rbMix == "ALL") { } else {
      checkboxGroupInput(ns("selected_targets"), "Targets",
        choices = unique(as.character(target_function()[, 1])), selected = NULL, inline = FALSE,
        width = NULL
      )
    }
  })

  output$mixingOutput <- renderDT({
    mixingOutput()
  })

  mixingOutput <- eventReactive(input$ui_src_applymix, {

    # create cluster
    library(parallel)
    cl <- makeCluster(detectCores() - 1)


    ns <- session$ns


    l <- corrected_function()[[1]] # list of source data

    DFA_l <- dfaList_x() # data frame of dfa
    targetD <- as.data.frame(target_function())

    finalDat <- NULL

    if (input$rbMix == "ALL") {

    } else {
      l <- l[which(target_function()[, 1] %in% input$selected_targets)]
      targetD <- targetD[which(targetD[, 1] %in% input$selected_targets), ]
      DFA_l <- DFA_l[which(targetD[, 1] %in% input$selected_targets), ]
    }

    modelOutput <- NULL

    ##globals
    target_glob <<- targetD
    dfa_glob <<- DFA_l
    mylist_glob <<- l
    
    for (i in seq(1, length(l))) { # length(l))) {
      target <- targetD[i, -c(1, 2)]
      DFA <- DFA_l[i, ]
      x <- l[[i]]
      uniSource <- unique(x[, 2])
      uniSource <- as.character(uniSource)
      ui_src_split <- input$ui_src_split

      # use function that was sourced with parallel processing
      if (input$mcsimulations > 1) {
        parReplicate <- function(cl, n, expr, simplify = TRUE, USE.NAMES = TRUE)
          parSapply(cl, integer(n), function(i, ex) eval(ex, envir = .GlobalEnv),
            substitute(expr),
            simplify = simplify, USE.NAMES = USE.NAMES
          )
        clusterExport(cl, list(
          "UseUnMixing", "DFA", "FunFunc", "uniSource", "x", "targetD",
          "ui_src_split", "getSubsetmean", "findMean", "target", "i", "convert", "zero_const", "neg_glob"
        ), envir = environment())
        output <- parReplicate(cl, input$mcsimulations, FunFunc(), simplify = "matrix")
      } else {
        # define single function
        FunFunc_s <- function() {
          inputTrain <- NULL
          inputValidate <- NULL
          #
          #           dat_transform <- function(x) {
          #             if (all(x < 0)) {
          #               x <- x * (-1)
          #             } else if (any(x <= 0)) {
          #               const <- abs(min(x))
          #               formulas <- strsplit(neg_glob, ',')
          #               x <- eval(parse(text = formulas[[1]][1]))
          #               x
          #             } else {
          #               x
          #             }
          #           }

          for (i2 in seq(1, length(uniSource))) {
            dat <- x[which(x[, 2] == uniSource[i2]), ]
            dat <- convert(dat, neg_glob, zero_const)

            train_index <- sample(1:nrow(dat), nrow(dat) * ui_src_split)
            training_dat <- dat[train_index, ]
            validate_dat <- dat[-train_index, ]
            inputTrain <- rbind(inputTrain, training_dat)
            inputValidate <- rbind(inputValidate, validate_dat)
          }

          datas <- getSubsetmean(inputTrain[, -1])

          DFA <- DFA[(which(colnames(DFA) %in% colnames(datas)))]
          DFA <- DFA[, colSums(DFA != 0) > 0]
          target <- target[, which(names(target) %in% colnames(DFA))]
          datas <- datas[, which(colnames(datas) %in% colnames(DFA))]
          dat <- inputValidate [, -c(1, 2)]
          dat <- dat[, which(names(dat) %in% colnames(DFA))]
          matchNames <- match(colnames(dat), colnames(target))
          dat <- rbind(dat, target[matchNames])
          # dat <- apply(dat, 2, dat_transform)
          dat <- data.matrix(dat)
          target <- dat[nrow(dat), ]
          rownames(dat) <- c(as.character(inputValidate[, 1]), as.character(targetD[i, 1]))
          
          
          d <- UseUnMixing(target, datas, DFA, method = "Nelder-Mead")
          d <- round(d, 4)
          d <- c(d, targetD[i, 1])
          names(d) <- NULL
          names(d) <- c(rownames(datas), "GOF", "target")
          return(d)
        }

        output <- replicate(input$mcsimulations, FunFunc_s(), simplify = "matrix")
      }
      modelOutput <- rbind(modelOutput, t(output))
    }

    stopCluster(cl)
    modelOutput
  })






  output$targetPlot <- renderUI({
    ns <- session$ns

    req(mixingOutput())
    dat <- as.data.frame(mixingOutput())
    un <- unique(as.character(dat$target))
    # selectInput('targetPlot', 'Select target', unique(dat[,ncol(dat)]), selected = NULL)
    selectInput(ns("targetPlot"), "Select target", un, selected = NULL)
  })

  output$trg_mixing_plot <- renderPlot({
    req(mixingOutput())
    req(input$targetPlot)

    dat_mixing_glob <<- mixingOutput ()
    dat <- as.data.frame(mixingOutput())
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

  output$select_violin_plot <- renderUI({
    ns <- session$ns
    s <- select_violin_plot_f()
    selectInput(ns("select_violin"), "Elements to plot", choices = s, selected = s[-length(s)], multiple = TRUE)
  })

  select_violin_plot_f <- reactive({
    req(mixingOutput())
    req(input$targetPlot)
    dat <- as.data.frame(mixingOutput())
    s <- names(dat[, -ncol(dat)])
    s
  })
  
  
  output$mybarplot <- renderPlot ({
    req(mixingOutput())
    #req(input$targetPlot)
    
    dat <- as.data.frame(mixingOutput())
    dat <- dat[,which (!colnames(dat) %in% ("GOF"))]
    dat <- melt(dat, id.vars = c("target"))

    datas <- dat
    colnames(datas) <- c("Target", "Class", "Proportion")
    datas[, 2] <- as.factor(datas[, 2])
    datas$Proportion <- as.numeric(as.character(datas$Proportion))
    
    # datas
    # datas %>%
    #   group_by(Class) %>%
    #   summarize(mean_size = mean(Proportion, na.rm = TRUE))
    # 
    # datas %>% dplyr::group_by (Target, Class) %>% dplyr::summarize (mean(Proportion))
    
    
    p <- ggplot(datas, aes(x = Target, y = Proportion, fill = Class)) +
      geom_bar(stat = "identity", position = "fill") + ggtitle("Proportion of sediment fluxes")
    p
  })
  
}
