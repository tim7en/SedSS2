source("correction_function.R")
source("check_correction.R")

correction_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      div(
        style = "width: 70%;",
        sidebarPanel(
          tabsetPanel(
            tabPanel(
              "Initial input",
              uiOutput(ns("corBut")),
              uiOutput(ns("ui_formulas_selected")),
              uiOutput(ns("ui_src_adjustfor")),
              uiOutput(ns("ui_src_remove")),
              uiOutput(ns("ui_src_shapiro")),
              uiOutput(ns("ui_src_cor")),
              uiOutput(ns("ui_src_applycor")),
              br(),
              br()
            ),
            tabPanel(
              "Plot Control",
              tags$div(
                HTML("<strong>MODELS PLOT CONTROL</strong>")
              ),
              br(),
              uiOutput(ns("targets")),
              uiOutput(ns("sources")),
              uiOutput(ns("element")),
              uiOutput(ns("formulas")),
              uiOutput(ns("corFor")),
              br(),
              br(),
              tags$div(
                HTML("<strong>CORRECTED PLOT CONTROL</strong>")
              ),
              br(),
              uiOutput(ns("ui_src_targets")),
              uiOutput(ns("xvar"))
              ,
              uiOutput(ns("independent_predictor"))
            )
          )
        ), width = 2
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
                  withSpinner(DTOutput(ns("src_corr_output"))), style = "height:'500px'; overflow-y: scroll;overflow-x: scroll;"
                  # ,
                  # withSpinner(DTOutput(ns("target_correction_formulas"))), style = "height:'auto'; overflow-y: scroll;overflow-x: scroll;"
                )
              )
            ),
            tabPanel(
              "Table",
              fluidPage(
                box(
                  title = "Top pick", status = "success", height =
                    "auto", width = "12", solidHeader = T,
                  column(
                    width = 12,
                    withSpinner(DTOutput(ns("correct_formulasdef_output"))), style = "height:'500px'; overflow-y: scroll;overflow-x: scroll;"
                  )
                )
              )
            ),
            tabPanel(
              "Confirm",
              fluidPage(
                box(
                  title = "Formulas", status = "success", height =
                    "auto", width = "12", solidHeader = T,
                  column(
                    width = 12,
                    withSpinner(DTOutput(ns("corr_formulas_output"))), style = "height:'500px'; overflow-y: scroll;overflow-x: scroll;"
                  )
                ),
                box(
                  title = "Selected", status = "primary", height =
                    "auto", width = "12", solidHeader = T,
                  column(
                    width = 12,
                    withSpinner(DTOutput(ns("correct_formulas_output"))), style = "height:'500px'; overflow-y: scroll;overflow-x: scroll;"
                  )
                )
              )
            ),
            navbarMenu(
              "Plots, Model",
              tabPanel(
                "Model plots",
                fluidPage(
                  box(
                    title = "Plot of response vs predictor ", status = "success", height =
                      800, width = 12, solidHeader = T,
                    div(withSpinner(plotlyOutput(ns("modelPlot"))), align = "center")
                  )
                )
              ),
              tabPanel(
                "lm plot",
                fluidPage(
                  box(
                    title = "Regressions Test Plots", status = "success", height =
                      "800px", width = 12, solidHeader = T,
                    withSpinner(plotOutput(ns("regression_plot_output"))), style = "height:'800'; overflow-y: scroll;overflow-x: scroll;"
                  )
                )
              ),
              tabPanel(
                "QQplot",
                fluidPage(
                  box(
                    title = "QQ plot", status = "success", height =
                      "800px", width = 12, solidHeader = T,
                    withSpinner(plotOutput(ns("QQplot")))
                  )
                )
              )
            ),
            tabPanel(
              "Data",
              fluidPage(
                box(
                  title = "Corrected Source Data", status = "success", height =
                    "auto", width = "12", solidHeader = T,
                  column(
                    width = 12,
                    uiOutput (ns('ui_src_targets2')),

                    withSpinner(DTOutput(ns("correct_src_selected_output"))), style = "height:'600'; overflow-y: scroll;overflow-x: scroll;"
                  )
                )
              )
            ),
            tabPanel(
              "Slopes",
              fluidPage(
                box(
                  title = "Slopes", status = "primary", height = "auto", solidHeader = T, width = "12",
                  column(
                    width = 12,
                    withSpinner(DTOutput(ns("correct_src_slopes_output"))), style = "height:'600'; overflow-y: scroll;overflow-x: scroll;"
                  )
                )
              )
            ),
            navbarMenu(
              "Plots, Corrected",
              tabPanel(
                "Transformed~Original, 1-1 Plot",
                withSpinner(scatterD3Output(ns("scatterplot1")))
              ),
              tabPanel(
                "Adjusted Plot vs Correction",
                withSpinner(scatterD3Output(ns("scatterplot2")))
              ),
              tabPanel(
                "Transformed and untransformed plots",
                
                div(plotlyOutput(ns('TransformedPlot')), align = "center")
                # box(
                #   title = "QQ plot of Original Data", status = "success", height = "auto", width = 6, solidHeader = T,
                #   withSpinner(plotlyOutput(ns("getorigQQval"))), style = "height:630px;overflow-y: scroll;overflow-x: scroll;"
                # ),
                # box(
                #   title = "QQ plot of Transformed Data", status = "success", height = "auto", width = 6, solidHeader = T,
                #   withSpinner(plotlyOutput(ns("gettransforQQval"))), style = "height:630px;overflow-y: scroll;overflow-x: scroll;"
                # )
              )
            )
          )
        )
      )
    )
  )
}

correction_server <- function(input, output, session, datas, target) {

  # radiobutton, select correct or no correct
  output$corBut <- renderUI({
    ns <- session$ns
    radioButtons(
      ns("corBut"), "Correct for any of the elements?",
      c(
        "Correct" = "Cor",
        "No correction" = "noCor"
      ),
      selected = "Cor"
    )
  })

  # keep only columns in the source that are present in the target
  src_ref_function <- reactive({
    req(datas())
    req(target())
    datas()[, which(names(datas()) %in% names(target()))]
  })

  # select columns to adjust for
  output$ui_src_adjustfor <- renderUI({
    req(datas())
    req(target())
    ns <- session$ns
    selectInput(ns("slcC"), "Select columns with size, toc or together", choices = names(src_ref_function())[-c(1, 2)], multiple = TRUE, width = "100%")
  })

  # option to pick columns that will not be used for correction
  output$ui_src_remove <- renderUI({
    req(src_ref_function())
    req(datas())
    req(target())
    req(input$slcC)
    ns <- session$ns
    if (!is.null(input$slcC)) {
      drops <- c(names(src_ref_function())[c(1, 2)], input$slcC)
    } else {
      drops <- names(src_ref_function())[c(1, 2)]
    }
    selectInput(ns("slcR"), "Select columns to remove from correction", choices = names(src_ref_function())[which(!names(src_ref_function()) %in% drops)], multiple = TRUE, width = "100%")
  })

  # select p value threshold of normality for residuals
  output$ui_src_shapiro <- renderUI({
    req(src_ref_function())
    ns <- session$ns
    sliderInput(ns("ui_src_shapiro"), "Shapiro-Wilk Univariate Normality Test p-value:", value = 0.05, min = 0.001, max = 1, step = 0.01)
  })

  # sliderinput for corrplot R threshold
  output$ui_src_cor <- renderUI({
    req(src_ref_function())
    ns <- session$ns
    sliderInput(ns("ui_src_cor"), "Adjusted R2", value = 0.6, min = 0.1, max = 0.99, step = 0.1, animate = F)
  })

  # action button, apply
  output$ui_src_applycor <- renderUI({
    req(src_ref_function())
    ns <- session$ns
    actionButton(ns("ui_src_applycor"), "Apply")
  })

  # if action button clicked, call for correct.func to try all possible combinations
  src_corr_function <- eventReactive(input$ui_src_applycor, {
    if (length(input$slcC) == 0) {
      return(NULL)
    }
    if (input$corBut == "Cor") {
      if (length(input$slcC) > 2) {
        return(NULL)
      }
      datas <- src_ref_function()
      datas <- convert(datas, neg_glob, zero_const) # check for negatives (mixed) and replace 0 with constant
      
      datas <- correct.func(as.data.frame(datas), input$ui_src_cor, input$ui_src_shapiro, input$slcC, input$slcR)
      
      if (is.null(datas)) {
        return(NULL)
      }
      datas <- datas[, -which(names(datas) %in% c("formula"))]
      datas <- datas[!duplicated(datas[, 5]), ]
      datas$id <- seq(1, nrow(datas))
      if (!is.null(datas)) {
        datas
      } else {
      }
    } else {
      NULL
    }
  })

  # function to check all possible combinations, and keep only one that pass check
  target_correction_formulas_function <- reactive({
    req(src_ref_function()) # source data
    if (!is.null(src_corr_function())) {
      formulas <- src_corr_function()
      datas <- src_ref_function()
      datas <- convert(datas, neg_glob, zero_const)
      datas <- datas[, which(!colnames(datas) %in% input$slcR)]
      target <- as.data.frame(target_function())
      target <- target[, which(!colnames(target) %in% input$slcR)]
      y <- formulas
      x <- as.data.frame(datas)
      datOutput <- NULL

      for (i in seq(1, dim(target)[1])) {
        dat <- check_correction(x, target[i, ], y, input$slcC)
        dat <- dat [!duplicated(dat[, 5]), ]
        if (dim(dat)[1]>0){
          dat$target <- as.character(target[i, 1])
        }
        datOutput <- rbind(datOutput, dat)
      }
      
      if (length(input$slcC) > 1){
        datOutput <- filter_regtable (datOutput)
      }
      
      return(datOutput)
      
      
    } else {
      NULL
    }
  })

  # output table (all of them, page 1)

  datDT <- eventReactive(input$ui_src_applycor, {
    req(src_corr_function())

    if (!is.null(src_corr_function())) {
      datas <- target_correction_formulas_function()
      colnames(datas)[5] <- "Formula"
      datas[, 5] <- (gsub("I(datas$", replacement = "(", datas[, 5], fixed = T))
      datas[, 5] <- (gsub("datas$", replacement = "", datas[, 5], fixed = T))
      datas[, 5] <- (gsub("I", replacement = "", datas[, 5], fixed = T))
      datas[, 5] <- (gsub("logI", replacement = "log", datas[, 5], fixed = T))
      if (!is.null(datas)) {
        datas
      } else {
      }
    } else {
      NULL
    }
  })

  output$src_corr_output <- renderDT({
    req(src_corr_function())
    dat <- datDT()
    # df[ , -which(names(df) %in% c("z","u"))]
    dat <- dat[, -which(names(dat) %in% ("grade"))]
  })


  # main function to sort data and just have an output table
  corr_formulas_function_fn <- reactive({
    if (!is.null(src_corr_function())) {
      #dat <- target_correction_formulas_function()

      f <- as.data.frame (target_correction_formulas_function())
      # val_table <- dat
      # varr <- val_table %>% group_by(target, source, element) # group_by(source, element) #group_by (target, source, element)
      # 
      # f <- varr %>% dplyr::arrange(-desc(residualsSD), desc(Cooks), .by_group = TRUE)
      # f <- f %>%
      #   dplyr::group_by(target, source, element) %>%
      #   dplyr::mutate(rank = rank(-desc(residualsSD), ties.method = "first"))
      # f <- f %>% group_by(target, source, element) %>% filter(row_number() <= 10)
      f <- f[, -which(names(f) %in% c("formula", "grade", "Cooks", "residualsSD", "p-eq", "p-resi", "p.eq", "p.resi"))]
      # f <- as.data.frame(f)
      
      #f <- filter_regtable(dat)
      

      return(f)
    } else {
      NULL
    }
  })

  # Formulas Output function, continuation(cleaning the formula, removing "text")
  corr_formulas_function <- reactive({
    f <- data.frame(corr_formulas_function_fn())
    f[, 3] <- (gsub("I(datas$", replacement = "(", f[, 3], fixed = T))
    f[, 3] <- (gsub("datas$", replacement = "", f[, 3], fixed = T))
    f[, 3] <- (gsub("I", replacement = "", f[, 3], fixed = T))
    f[, 3] <- (gsub("logI", replacement = "log", f[, 3], fixed = T))
    f[, 1] <- as.factor(f[, 1])
    f[, 3] <- as.factor(f[, 3])
    return(f)
  })

  # Output table, FORMULAS top 10, sorted (ready for selection)
  output$corr_formulas_output <- renderDT({
    if (!is.null(corr_formulas_function())) {
      dat <- corr_formulas_function()[, c(3, 2, 1, 6, 4, 5)]
      dat[, 4] <- as.factor(dat[, 4])
      names(dat)[1] <- ('Formulas')
      datatable(dat, filter = "top", options = list(
        pageLength = 5, autoWidth = T
      ))
    } else {
      NULL
    }
  })

  # radio button serve as indicator of user choice or default choice
  output$ui_formulas_selected <- renderUI({
    req(src_ref_function())
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

  # if the radio button on default, in the function output top picks, else selected user choice
  correct_formulas_function <- reactive({
    req(input$ui_formulas_selected)
    if (input$ui_formulas_selected == "def") { # if it is default, have as an output top table
      datas <- correct_formulasdef_function()
    } else {
      datas <- corr_formulas_selected_function() # if it is user choice, have an output selected
    }
    datas
  })

  # Top pick will be (correct_formulasdef_function ())
  correct_formulasdef_function <- reactive({
    dat <- corr_formulas_function()
    val_table <- dat
    varr <- val_table %>% group_by(target, source, element) # group_by(source, element) #group_by (target, source, element)

    f <- varr %>% dplyr::arrange(desc(R2), .by_group = TRUE)
    f <- f %>%
      dplyr::group_by(target, source, element) %>%
      dplyr::mutate(rank = rank(desc(R2), ties.method = "first"))
    #f <- f %>% group_by(target, source, element) %>% filter(row_number() <= 10) #extracts only top 10 for each group
    
    
    datas <- f[which(f$rank == 1), ]
    datas <- as.data.frame (datas)
    names (datas)[3] <- 'Formulas'
    datas
  })

  # table of top picks
  output$correct_formulasdef_output <- renderDT({
    dat <- correct_formulasdef_function()
    dat[, 6] <- as.factor(dat[, 6])
    datatable(dat, filter = "top", options = list(
      pageLength = 5, autoWidth = T
    ))
  })


  # table of final picks
  output$correct_formulas_output <- renderDT({
    dat <- correct_formulas_function()
    dat[, 6] <- as.factor(dat[, 6])
    datatable(dat, filter = "top", options = list(
      pageLength = 5, autoWidth = T
    ))
  })


  corrected_function <- reactive({
    if (input$corBut == "Cor") {
      req(src_ref_function()) # source data
      req(target_function()) # target data
      # req(input$ui_src_targets)


      datas <- src_ref_function()
      datas <- datas[, which(!colnames(datas) %in% input$slcR)]
      target <- as.data.frame(target_function())
      target <- target[, which(!colnames(target) %in% input$slcR)]

      x <- as.data.frame(datas)

      dat_formulas1 <- correct_formulas_function()
      datas_main <- target_correction_formulas_function()


      output <- list()
      slopes <- list()
      drops <- list()
      
      dat_glob <<- datas_main
      dat_formulas1_glob <<- dat_formulas1
      x_glob <<- x
      target_glob <<- target
      

      for (i in seq(1, dim(target)[1])) {
        dat <- datas_main[which(datas_main$target %in% as.character(target[i, 1])), ]
        
        if (dim(dat)[1] == 0){        
          datas <- src_ref_function()
          output [[i]] <- datas
          next
        }
        
        dat_formulas_sub <- dat_formulas1[which(dat_formulas1$target %in% as.character(target[i, 1])), ]
        sub_formulas <- dat[which(dat$id %in% dat_formulas_sub$id), ]
        sub_y <- cbind(sub_formulas[, 1], 0, sub_formulas[, -c(1, ncol(sub_formulas))])
        
        negatives <- colnames(x)[-c(1, 2)][sapply(x[, -c(1, 2)], function(x) min(x)) <= 0]
        positives <- colnames(x)[-c(1, 2)][sapply(x[, -c(1, 2)], function(x) max(x)) >= 0]
        
        negatives_glob <<- negatives
        positives_glob <<- positives
        mixed <- negatives[which(negatives %in% positives)]
        mixed_glob <<- mixed
        
        x <- convert(x, neg_glob, zero_const)
        dat <- correct(x, target[i, ], sub_y, input$slcC)
        
        dat[[1]] <- inverse_neg_glob (dat[[1]], neg_glob)
        dat[[1]][,which (colnames(dat[[1]]) %in% negatives)] <- dat[[1]][,which (colnames(dat[[1]]) %in% negatives)]-zero_const
        
        
        output[[i]] <- cbind(dat[[1]], data.frame(src_ref_function()[, c(input$slcR)]))

        rbind.na <- qpcR:::rbind.na

        slopes[[i]] <- do.call("rbind.na", dat[[2]])
        slopes[[i]] <- cbind(slopes[[i]], paste0("source: ", i))

        if (length(dat[[3]]) > 0) {
          drops [[i]] <- dat[[3]]
        } else {
          drops[[i]] <- NA
        }
      }

      y <- slopes

      if (length(input$slcC) > 1) {
        slopes.DT <- do.call("rbind.na", y)
        colnames(slopes.DT) <- c("var1", "var2", "var1*var2", "source")
      } else {
        slopes.DT <- do.call("rbind.na", y)
        colnames(slopes.DT) <- c("var1", "source")
      }
      
      output_glob <<- output

      return(list(output, slopes.DT, drops))
    } else {
      req(src_ref_function())
      req(target_function())
      output <- list()
      for (i in seq(1, nrow(target_function()))) {
        datas <- src_ref_function()
        output [[i]] <- datas
      }
      return(list(output, NULL, NULL))
    }
  })






  # plot 1
  output$ui_corvar_plot <- renderUI({
    req(src_ref_function())
    ns <- session$ns
    if (!is.null(input$ui_formulas_selected)) {
      if (input$ui_formulas_selected == "def") {
        selectInput(ns("ui_corvar_plot"), label = "Select Element", choices = correct_formulas_function()[, 3])
      } else {
        selectInput(ns("ui_corvar_plot"), label = "Select Element", choices = correct_formulas_function()[, 3])
      }
    } else {}
  })

  # regressions plot function
  regression_plot_function <- reactive({
    req(correct_formulasdef_function())
    datas <- src_ref_function()
    datas <- convert(datas, neg_glob, zero_const)

    if (!is.null(input$ui_formulas_selected)) {
      if (input$ui_formulas_selected == "def") {
        if (!is.null(input$ui_corvar_plot)) {
          sclass <- correct_formulasdef_function()[which(correct_formulasdef_function()[, 3] == input$ui_corvar_plot), 4]
          sclass <- as.character(sclass)
          datas <- datas[which(datas[, 2] == sclass), ]
        } else {}
      } else {
        if (!is.null(input$ui_corvar_plot)) {
          sclass <- corr_formulas_selected_function()[which(corr_formulas_selected_function()[, 3] == input$ui_corvar_plot), 4]
          sclass <- as.character(sclass)
          datas <- datas[which(datas[, 2] == sclass), ]
          datas
        } else {}
      }
    }
    datas
  })

  # selected formulas after corretion #take the output of top choices, and apply selection
  corr_formulas_selected_function <- reactive({
    corr_formulas_function()[input$corr_formulas_output_rows_selected, ] # check
  })

  corr_formulas_validtab_formula <- reactive({
    if (!is.null(src_corr_function())) {
      dat1 <- src_corr_function()
      dat2 <- corr_formulas_function()
      dat <- dat1[which(dat1$id %in% dat2$id), ]
      dat <- cbind(dat[, 1], 0, dat[, 2:ncol(dat)])
      dat
    }
    else {
      NULL
    }
  })

  # regressions plot output
  output$regression_plot_output <- renderPlot({
    req(input$targets)
    req(input$sources)
    req(input$element)
    req(input$formulas)
    # req (input$secondCor)


    slcC <- input$slcC
    datas <- src_ref_function()
    datas <- convert(datas, neg_glob, zero_const)

    if (length(slcC) > 1) {
      if (dim(datas)[1] == 0) {
        return(NULL)
      } # fixes error when we switch default to user
      var1 <- as.numeric(datas[, slcC[1]])
      var2 <- as.numeric(datas[, slcC[2]])
    } else {
      var1 <- as.numeric(datas[, slcC])
      var2 <- NULL
    }

    datas[, 3:dim(datas)[2]] <- apply(datas[, 3:dim(datas)[2]], 2, as.numeric)

    f <- input$formulas
    f <- sub("^.", "I(datas$", f)
    f <- sub("(var1", "I(var1", f, fixed = T)
    f <- sub("(var2", "I(var2", f, fixed = T)
    f <- sub("logI", "log", f, fixed = T)
    f <- sub("I(datas$og(", "log(datas$", f, fixed = T)
    fit <- lm(eval(parse(text = f)))

    par(mfrow = c(1, 3))
    cooksd <- cooks.distance(fit)
    plot(cooksd, cex = 2, main = "Influential Obs by Cooks distance")
    lines(lowess(cooksd), col = "blue")
    plot(fitted(fit), residuals(fit))
    lines(lowess(fitted(fit), residuals(fit)))
    title("Residual vs Fit. value ", col = "red")
    acf(residuals(fit), main = "")
    title("Residual Autocorrelation Plot")
  })

  output$ui_corvar_plot2 <- renderUI({
    req(src_ref_function())
    ns <- session$ns
    if (!is.null(input$ui_formulas_selected)) {
      if (input$ui_formulas_selected == "def") {
        selectInput(ns("ui_corvar_plot2"), label = "Select Element", choices = correct_formulas_function()[, 3])
      } else {
        selectInput(ns("ui_corvar_plot2"), label = "Select Element", choices = correct_formulas_function()[, 3])
      }
    } else {}
  })


  output$targets <- renderUI({
    ns <- session$ns
    req(target_correction_formulas_function())
    dat <- target_correction_formulas_function()
    
    selectInput(ns("targets"), "Targets", choices = as.factor(dat[, 11]), selected = dat[1, 11])
  })


  output$sources <- renderUI({
    ns <- session$ns
    req(input$targets)
    req(target_correction_formulas_function())
    dat <- target_correction_formulas_function()
    subset_df <- dat[which(as.character(dat[, 11]) == as.character(input$targets)), ]
    
    selectInput(ns("sources"), "Sources", choices = as.factor(subset_df[, 7]), selected = subset_df[1, 7])
  })

  output$element <- renderUI({
    ns <- session$ns
    req(input$sources)
    req(input$targets)
    req(target_correction_formulas_function())
    dat <- target_correction_formulas_function()
    subset_df <- dat[which(as.character(dat[, 11]) == as.character(input$targets)), ]
    subset_df <- subset_df[which(as.character(subset_df[, 7]) == as.character(input$sources)), ]
    
    selectInput(ns("element"), "Elements", choices = as.factor(subset_df[, 1]), selected = subset_df[1, 1])
  })

  output$formulas <- renderUI({
    ns <- session$ns
    req(input$sources)
    req(input$targets)
    req(target_correction_formulas_function())
    req(input$element)
    dat <- target_correction_formulas_function()
    subset_df <- dat[which(as.character(dat[, 11]) == as.character(input$targets)), ]
    subset_df <- subset_df[which(as.character(subset_df[, 7]) == as.character(input$sources)), ]
    subset_df <- subset_df[which(as.character(subset_df[, 1]) == as.character(input$element)), ]
    subset_df[, 5] <- (gsub("I(datas$", replacement = "(", subset_df[, 5], fixed = T))
    subset_df[, 5] <- (gsub("datas$", replacement = "", subset_df[, 5], fixed = T))
    subset_df[, 5] <- (gsub("I", replacement = "", subset_df[, 5], fixed = T))
    subset_df[, 5] <- (gsub("logI", replacement = "log", subset_df[, 5], fixed = T))
    subset_df[, 5] <- as.factor(subset_df[, 5])

    selectInput(ns("formulas"), "Formulas", choices = as.factor(subset_df[, 5]), selected = subset_df[1, 5])
  })


  output$corFor <- renderUI({
    ns <- session$ns
    req(input$sources)
    req(input$targets)
    req(target_correction_formulas_function())
    req(input$element)
    req(length(input$slcC) > 1)
    
    selectInput(ns("secondCor"), "Plot element", choices = input$slcC, selected = NULL)
  })


  # Interaction and regression plots
  output$modelPlot <- renderPlotly({
    req(input$targets)
    req(input$sources)
    req(input$element)
    req(input$formulas)
    # req (input$secondCor)

    slcC <- input$slcC
    datas <- src_ref_function()
    datas <- convert(datas, neg_glob, zero_const)

    if (length(slcC) > 1) {
      if (dim(datas)[1] == 0) {
        return(NULL)
      } # fixes error when we switch default to user
      var1 <- as.numeric(datas[, slcC[1]])
      var2 <- as.numeric(datas[, slcC[2]])
    } else {
      var1 <- as.numeric(datas[, slcC])
      var2 <- NULL
    }

    datas[, 3:dim(datas)[2]] <- apply(datas[, 3:dim(datas)[2]], 2, as.numeric)

    f <- input$formulas
    f <- sub("^.", "I(datas$", f)
    f <- sub("(var1", "I(var1", f, fixed = T)
    f <- sub("(var2", "I(var2", f, fixed = T)
    f <- sub("logI", "log", f, fixed = T)
    f <- sub("I(datas$og(", "log(datas$", f, fixed = T)
    fit <- lm(eval(parse(text = f)))
    

    if (length(fit$model) > 2) {
      if (dim(datas)[1] == 0) {
        return(NULL)
      } # fixes error when we switch default to user
      response <- fit$model[[1]]
      var1 <- fit$model[[2]]
      var2 <- fit$model[[3]]
    } else {
      response <- fit$model[[1]]
      var1 <- fit$model[[2]]
      var2 <- NULL
    }

    if (!is.null(var2)) {
      ind <- which(input$slcC == input$secondCor)
      var1 <- fit$model[[ind + 1]]
      slcC <- input$secondCor
      p <- ggplot(datas, aes(y = response, x = var1)) + geom_point() + stat_smooth(method = "lm", se = TRUE, fill = "lightblue")
      p <- p + labs(x = slcC, y = input$element)
      ggplotly(p, height = 600)
    } else {
      slcC <- input$secondCor
      p <- ggplot(datas, aes(y = response, x = var1)) + geom_point() + stat_smooth(method = "lm", se = TRUE, fill = "lightblue")
      p <- p + labs(x = slcC, y = input$element)
      ggplotly(p, height = 600)
    }
  })

  output$QQplot <- renderPlot({
    req(input$targets)
    req(input$sources)
    req(input$element)
    req(input$formulas)

    slcC <- input$slcC
    datas <- src_ref_function()
    datas <- convert(datas, neg_glob, zero_const)

    if (length(slcC) > 1) {
      if (dim(datas)[1] == 0) {
        return(NULL)
      } # fixes error when we switch default to user
      var1 <- as.numeric(datas[, slcC[1]])
      var2 <- as.numeric(datas[, slcC[2]])
    } else {
      var1 <- as.numeric(datas[, slcC])
      var2 <- NULL
    }

    datas[, 3:dim(datas)[2]] <- apply(datas[, 3:dim(datas)[2]], 2, as.numeric)
    
    f <- input$formulas
    f <- sub("^.", "I(datas$", f)
    f <- sub("(var1", "I(var1", f, fixed = T)
    f <- sub("(var2", "I(var2", f, fixed = T)
    f <- sub("logI", "log", f, fixed = T)
    f <- sub("I(datas$og(", "log(datas$", f, fixed = T)
    fit <- lm(eval(parse(text = f)))
    # attach (fit)
    response <- residuals(fit)
    qqPlot(as.numeric(response))
  })


  target_function <- reactive({
    target()[, which(names(target()) %in% names(src_ref_function()))]
  })

  output$ui_src_targets <- renderUI({
    req(src_ref_function())
    req(src_corr_function())
    req(correct_formulas_function())
    dat <- correct_formulas_function()
    dat <- dat[order(dat$target),]
    s <- as.character(unique(dat$target))
    
    ns <- session$ns
    
    
    selectInput(ns("ui_src_targets"), "Target ID", choices = s, selected = NULL)
  })

  output$ui_src_targets2 <- renderUI({
    req(src_ref_function())
    req(src_corr_function())
    req(correct_formulas_function())
    dat <- correct_formulas_function()
    dat <- dat[order(dat$target),]
    s <- as.character(unique(dat$target))
    
    ns <- session$ns
    
    
    selectInput(ns("ui_src_targets2"), "Target ID", choices = s, selected = NULL)
  })

  output$correct_src_selected_output <- renderDT({
    correct_src_selected_function()
  })

  correct_src_selected_function <- reactive({
    req(corrected_function())
    #corrected_glob <<- corrected_function ()
    req(input$ui_src_targets2)
    ind <- which(as.character(unique(target_function()[, 1])) %in% as.character(input$ui_src_targets2))
    corrected_function() [[1]][[ind]]
  })

  output$correct_src_slopes_output <- renderDT({
    corrected_function() [[2]]
  })

  output$correct_src_drops_output <- renderDT({
    corrected_function() [[3]]
  })

  output$xvar <- renderUI({
    req(src_ref_function())
    req(input$ui_src_targets)
    ns <- session$ns
    formulas_tab <- correct_formulas_function()
    sub_tab <- formulas_tab[which(formulas_tab$target %in% input$ui_src_targets), ]
    s <- as.character(unique(sub_tab[, 1]))
    selectInput(ns("xvar"), "1-1 Plot, before & after correction", choices = s)
  })

  output$xvar2 <- renderUI({
    req(src_ref_function())
    req(input$ui_src_targets)
    ns <- session$ns
    formulas_tab <- correct_formulas_function()
    sub_tab <- formulas_tab[which(formulas_tab$target %in% input$ui_src_targets), ]
    s <- as.character(unique(sub_tab[, 1]))
    selectInput(ns("xvar2"), "Corrected, elements", choices = s)
  })

  output$independent_predictor <- renderUI({
    req(src_ref_function())
    req(input$ui_src_targets)
    ns <- session$ns
    selectInput(ns("independent_predictor"), "Corrected, for", choices = c(input$slcC))
  })

  output$scatterplot1 <- renderScatterD3({
    req(input$xvar)
    req(correct_src_selected_function())
    req(src_ref_function())
    mtdf <- correct_src_selected_function()
    dtorg <- src_ref_function()
    x <- dtorg[[input$xvar]]
    Sclass <- dtorg [, 2]
    y <- mtdf[[input$xvar]]
    
    
    scatterD3(
      x = x, y = y,
      labels_size = 9, point_opacity = 1, col_var = Sclass,
      lines = data.frame(slope = 1, intercept = Inf),
      transitions = T
    )
  })

  output$scatterplot2 <- renderScatterD3({
    req(input$independent_predictor)
    req(input$xvar)
    mtdf <- correct_src_selected_function()
    dtorg <- src_ref_function()
    x <- mtdf[[input$independent_predictor]]
    Sclass <- dtorg [, 2]
    y <- dtorg[[input$xvar]]


    scatterD3(
      x = x, y = y,
      labels_size = 9, point_opacity = 1, col_var = Sclass,
      transitions = T
    )
  })

  #new transformed plot
  output$TransformedPlot <- renderPlotly({
    #original data
    req(correct_src_selected_function())
    req(src_ref_function())
    dat_orig <- src_ref_function()
    req(input$xvar)
    req(input$independent_predictor)

    
    #transformed data
    req(correct_src_selected_function())
    req(src_ref_function())
    req(input$xvar)
    req(input$independent_predictor)
    dat_transformed <- correct_src_selected_function()
    dat_src <- src_ref_function()
    #colnames (dat_transformed)[2] <- 'Classes'

    dat <- rbind (dat_transformed, dat_orig)
    names(dat)[2] <- 'Classes'
    
    mygrid <- rep ('transformed', nrow (dat_transformed))
    mygrid <- c (mygrid, rep ('untransformed', nrow (dat_orig)))
    
    suppressMessages(attach(dat))
    suppressWarnings(assign("Response", dat[, c(input$xvar)]))
    suppressWarnings(assign("Predictor", dat[, c(input$independent_predictor)]))
    
    dat <- cbind (dat, mygrid)
    
    p <- ggplot(dat, aes(x = Predictor, y = Response, colour = mygrid)) +
      geom_point()+
      geom_smooth(method = lm)+
      theme(panel.spacing = unit(2, "lines"), legend.position = "bottom")
    p <- p+facet_wrap(Classes~., ncol = 2, scales = "free") #split in horizontal direction
    ggplotly(p, height = 800, width = 1000) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })
  
  # Facet plot of original data not transformed
  output$getorigQQval <- renderPlotly({
    req(correct_src_selected_function())
    req(src_ref_function())
    dat <- src_ref_function()
    req(input$xvar)
    req(input$independent_predictor)
    suppressMessages(attach(dat))
    suppressWarnings(assign("Response", dat[, c(input$xvar)]))
    suppressWarnings(assign("Predictor", dat[, c(input$independent_predictor)]))
    colnames(dat)[2] <- "Classes"

    p <- ggplot(dat, aes(x = Response, y = Predictor, colour = Classes)) +
      geom_point() +
      geom_smooth(method = lm) +
      facet_wrap(~Classes, ncol = 2, scales = "free") +
      theme(panel.spacing = unit(2, "lines"), legend.position = "bottom")
    ggplotly(p, height = 400, width = 600) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })

  # Facet plot of transformed data
  output$gettransforQQval <- renderPlotly({
    req(correct_src_selected_function())
    req(src_ref_function())
    req(input$xvar)
    req(input$independent_predictor)
    dat <- correct_src_selected_function()
    dat_src <- src_ref_function()
    dat3_glob <- dat_src
    suppressMessages(attach(dat_src))
    suppressWarnings(assign("Response", dat[, c(input$xvar)]))
    suppressWarnings(assign("Predictor", dat_src[, c(input$independent_predictor)]))
    colnames(dat_src)[2] <- "Classes"

    p <- ggplot(dat_src, aes(x = Response, y = Predictor, colour = Classes)) +
      geom_point() +
      geom_smooth(method = lm) +
      facet_wrap(~Classes, ncol = 2, scales = "free") +
      theme(panel.spacing = unit(2, "lines"), legend.position = "bottom")
    ggplotly(p, height = 400, width = 600) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })


  return(corrected_function)
}
