options(shiny.fullstacktrace = TRUE)
#remove warning
suppressWarnings({
  suppressPackageStartupMessages({
    library(shiny)
    library(DT)
    library(plyr)
    library(shinydashboard)
    library(zCompositions)
  })
})

# checks for negatives
is.negative <- function(x) {
  options(warn = -1)

  expr <- tryCatch({
    x <- try(as.numeric(x))
    if ((is.numeric(x) || is.integer(x))) {
      return(length(which(na.omit(x) < 0)))
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })

  options(warn = 1)
}

# checks for zeros
is.zero <- function(x) {
  options(warn = -1)

  expr <- tryCatch({
    x <- try(as.numeric(x))
    if ((is.numeric(x) || is.integer(x))) {
      return(length(which(x == 0)))
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })

  options(warn = 1)
}

# function to output summary table of a data frame
dfSummary <- function(x) {
  expr <- tryCatch({
    tabSummary <- NULL
    tabSummary <- rbind(tabSummary, sapply(x, function(x) class(x)))
    tabSummary <- rbind(tabSummary, sapply(x, function(x) sum(is.na(x))))
    tabSummary <- rbind(tabSummary, sapply(x, function(x) nlevels(x)))
    tabSummary <- rbind(tabSummary, apply(x, 2, is.negative))
    tabSummary <- rbind(tabSummary, apply(x, 2, is.zero))
    rownames(tabSummary) <- c("Type", "NAs", "Unique", "Negative", "Zeros")
  }, error = function(e) {
    return(NULL)
  })
  return(tabSummary)
}

#app sidebar
my_sidebar <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(
        "RENAME",
        "This page assists in checking and renaming sediment sources",
        br(),
        br(),
        uiOutput(ns("dat_levels")),
        uiOutput(ns("new_levels")),
        uiOutput(ns("accept_revalue"))
      ),
      tabPanel(
        "Negatives",
        uiOutput(ns("dat_negatives_par")),
        uiOutput(ns("dat_neg_text")),
        uiOutput(ns("accept_negconv")),
        br(),
        fluidRow(
          column(
            width = 6,
            infoBoxOutput(ns("ibox")),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            "Succesfull conversion result in a GREEN box, failed in a RED box."
          ),
          column(
            width = 6,
            "Current part will allow to define conversion applied to a mixed sign data (+, -) required to run certain functions. User should input method and inverse method. ",
            br(),
            br(),
            "For example: (x+1),(x-1).",
            br(),
            br(),
            "To combine separating them with:';'",
            br(),
            br(),
            "For example:",
            "(x+1),(x-1);(x/1000+1),(x-1)*1000 "
          )
        ),
        br(),
        br(),
        br()
      ),
      tabPanel(
        "Missing",
        uiOutput(ns("dat_missing")),
        uiOutput(ns("dat_det_lim")),
        uiOutput(ns("dat_missing_sub")),
        uiOutput(ns("dat_missing_adj")),
        uiOutput(ns("dat_miss_neg_sel")),
        uiOutput(ns("imp_rb")),
        uiOutput(ns("accept_missconv"))
      ),
      tabPanel(
        "Zero",
        uiOutput(ns("dat_miss_zero_sel")),
        uiOutput(ns("zero_const")),
        uiOutput(ns("accept_zeroconv")),
        br(),
        fluidRow(
          column(
            width = 6,
            infoBoxOutput(ns("ibox_zero")),
            br(),
            br(),
            br(),
            br()
          )
        )
      )
    )
  )
}

my_mainpage <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(
        "Data",
        box(
          width = 12,
          title = "Data table", solidHeader = TRUE, status = "primary",
          withSpinner(DTOutput(ns("dat_levels_dt"))),
          style = "height:'auto'; overflow-y: scroll;overflow-x: scroll;"
        ),
        uiOutput(ns("dt_lim_ui"))
      ),
      tabPanel(
        "Summary",
        box(
          title = textOutput(ns("dat_nrows")), status = "danger", height = "630", width = 12, solidHeader = T,
          withSpinner(DTOutput(ns("distTable"))),
          style = "height:'auto'; overflow-y: scroll;overflow-x: scroll"
        )
      )
    )
  )
}

sidebar_func <- function(input, output, session, datas) {

  ns <- session$ns
  dat_rv <- reactiveValues(dtab = NULL)
  

  # observeEvent(dat_sum(), {
    # showModal(modalDialog(
    #   title = "Important message",
    #   "This is an important message!"
    # ))
  # })
  
  # # the modal dialog where the user can enter the query details.
  # query_modal <- modalDialog(
  #   title = "Welcome!",
  #   "Welcome to the first version of sediment fingerprinting tool built in Shiny framework.
  #   Plase watch this short youtube video or close to advance.",
  #   easyClose = F,
  #   footer = tagList(
  #     actionButton("watch", "Watch Tutorial"),
  #     actionButton('watch_skip', 'Close')
  #   )
  # )
  # # Show the model on start up ...
  # showModal(query_modal)
  

  observe({
    req (datas())
    dat_rv$dtab <- datas()
  })

  output$dat_nrows <- renderText({
    paste0("Summary table of data with rows/columns: ", paste(nrow(dat_rv$dtab), ncol(dat_rv$dtab), sep = "/"))
  })

  output$distTable <- renderDT({
    data.frame(dat_sum())
  })

  dat_sum <- reactive({
    req(dat_rv$dtab)
    dfSummary(dat_rv$dtab)
  })

  output$dat_facts <- renderUI({
    ns <- session$ns
    req (datas())
    dat <- dat_sum()
    s <- colnames(dat)[which(as.numeric(as.matrix(dat[3, ])) > 0)]
    selectInput(ns("facts"), "Available sources", choices = c(s[2]), selected = c(s[2]))
  })

  output$dat_facts_mis <- renderUI({
    ns <- session$ns
    req (datas())
    dat <- dat_sum()
    s <- colnames(dat)[which(as.numeric(as.matrix(dat[3, ])) > 0)]
    selectInput(ns("facts2"), "Sources", choices = c(s[2]), selected = TRUE, multiple = T)
  })

  output$dat_negatives <- renderUI({
    ns <- session$ns
    dat <- dat_sum()
    selectInput(ns("negts"), "All values below 0", choices = colnames(dat)[which(as.numeric(as.matrix(dat[4, ])) > 0 & as.numeric(as.matrix(dat[4, ])) == nrow(dat_rv$dtab))], selected = NULL, multiple = TRUE)
  })

  output$dat_negatives_par <- renderUI({
    ns <- session$ns
    dat <- dat_sum()
    s <- colnames(dat)[which(as.numeric(as.matrix(dat[4, ])) > 0 & as.numeric(as.matrix(dat[4, ])) < nrow(dat_rv$dtab))]
    selectInput(ns("negts_par"), "Some values below 0", choices = c(s), selected = c(s), multiple = TRUE)
  })

  output$dat_neg_text <- renderUI({
    ns <- session$ns
    textInput(ns("neg_text"), "Method, default is (x/1000 +1)", value = "x/1000+1, (x-1)*1000")
  })

  output$dat_missing <- renderUI({
    ns <- session$ns
    dat <- dat_sum()
    s <- colnames(dat)[which(as.numeric(as.matrix(dat[2, ])) > 0)]
    selectInput(ns("missing"), "Values are missing", choices = c(s), selected = c(s), multiple = TRUE)
  })

  # output$dat_missing_sub <- renderUI({
  #   ns <- session$ns
  #   col_indx <- which(names(dat_rv$dtab) == input$facts2)
  #   sel_names <- rownames_missing()[, col_indx]
  #   selectInput(ns("missing_lvl"), "LEVELS", choices = c(levels(dat_rv$dtab[, col_indx]), "JOIN"), multiple = TRUE, selected = sel_names) # one that are missing values
  # })

  output$dat_missing_sub <- renderUI({
    ns <- session$ns
    sel_names <- rownames_missing()[, 2]
    selectInput(ns("missing_lvl"), "Sources", choices = c(levels(dat_rv$dtab[, 2])), multiple = TRUE, selected = sel_names) # one that are missing values
  })

  # output$dat_missing_adj <- renderUI({
  #   ns <- session$ns
  #   selectInput(ns("missing_adj"), "FILL", choices = c("MEAN", "MEDIAN", "IMPUTE"), selected = NULL, multiple = FALSE)
  # })

  output$dat_missing_adj <- renderUI({
    ns <- session$ns
    selectInput(ns("missing_adj"), "Option", choices = c("Impute"), selected = NULL, multiple = FALSE)
  })

  # could be replaced to textOutput instead of selectInput
  output$dat_miss_neg_sel <- renderUI({
    req(input$missing_adj)
    ns <- session$ns
    dat <- dat_sum()
    cl_remove <- unique(c(colnames(dat)[which(as.numeric(as.matrix(dat[4, ])) > 0)], colnames(dat)[which(as.numeric(as.matrix(dat[5, ])) > 0)]))
    if (input$missing_adj == "Impute") {
      clnames <- colnames(dat)[which(as.numeric(as.matrix(dat[2, ])) > 0)]
      clnames <- (clnames[which(!clnames %in% input$missing)])
      mychoices <- colnames(dat)[which (!colnames(dat) %in% c(clnames,input$missing))]
      mychoices <- (mychoices[-c(1, 2)])
      selectInput(ns("missing_neg"), "Excluded from imputation (negatives, zero)", choices = mychoices, selected = c(cl_remove, clnames), multiple = TRUE)
    } else {
      NULL
    }
  })

  output$dat_det_lim <- renderUI({
    req(input$missing_adj)
    req(input$impute_selected)
    ns <- session$ns
    if (input$impute_selected == "def") {
      if (input$missing_adj == "Impute") {
        textInput(ns("det_lim"), "Detection limits", value = "1")
      } else {
        NULL
      }
    } else { # This function is repsonsible for loading in the selected file
      fileInput(ns("datafile"), "Choose CSV file",
        accept = c("text/csv", "text/comma-separated-values,text/plain")
      )
    }
  })

  # could be replaced to textOutput instead of selectInput
  output$dat_miss_zero_sel <- renderUI({
    ns <- session$ns
    dat <- dat_sum()
    zero <- unique(c(colnames(dat)[which(as.numeric(as.matrix(dat[5, ])) > 0)], colnames(dat)[which(as.numeric(as.matrix(dat[5, ])) > 0)]))
    selectInput(ns("missing_zero"), "Zero found: ", choices = zero, selected = c(zero), multiple = TRUE)
  })

  output$zero_const <- renderUI({
    ns <- session$ns
    numericInput(ns("zero_const"), "Constant", 0.001, 0.001)
  })

  # This function is repsonsible for loading in the selected file
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv(infile$datapath)
  })

  output$dt_lim <- renderDT({
    req(input$impute_selected)
    if (input$impute_selected == "def") {
      return(NULL)
    }
    else {
      filedata()
    }
  })

  output$dt_lim_ui <- renderUI({
    ns <- session$ns
    req(input$impute_selected != "def")
    box(
      width = 12,
      title = "Detection limit", solidHeader = TRUE, status = "primary",
      DTOutput(ns("dt_lim")),
      style = "height:'auto'; overflow-y: scroll;overflow-x: scroll;"
    )
  })

  output$dat_levels <- renderUI({
    ns <- session$ns
    selectInput(ns("flevels"), "Available sources", choices = levels(dat_rv$dtab[, 2]), multiple = TRUE, selected = FALSE)
  })

  output$new_levels <- renderUI({
    ns <- session$ns
    req (datas())
    textInput(ns("new_names"), "Rename")
  })

  output$accept_revalue <- renderUI({
    ns <- session$ns
    actionButton(ns("accept_revalue_bt"), "Accept")
  })

  output$accept_negconv <- renderUI({
    ns <- session$ns
    actionButton(ns("accept_negconv_bt"), "Accept")
  })

  output$accept_zeroconv <- renderUI({
    ns <- session$ns
    actionButton(ns("accept_zeroconv_bt"), "Accept")
  })

  output$accept_missconv <- renderUI({
    ns <- session$ns
    actionButton(ns("accept_missconv_bt"), "Accept")
  })

  output$imp_rb <- renderUI({
    ns <- session$ns
    req(input$missing_adj)
    if (input$missing_adj == "Impute") {
      radioButtons(
        ns("impute_selected"), "Impute method :",
        c(
          "User type" = "def",
          "Upload matrix" = "sel_mat",
          "Upload table" = "sel_tab"
        )
      )
    } else {
      NULL
    }
  })

  # main function to rename factor levels
  dat_levels_rename <- eventReactive(input$accept_revalue_bt, {
    col_indx <- 2 # which(names(dat_rv$dtab) == input$facts) # find columns selected
    new_names <- try(unlist(strsplit(input$new_names, ","))) # unlist user input and use coma as separator

    if ((length(input$flevels) == 1) && (length(new_names) == 1)) { # if only one factor selected to rename and only one provided
      dat_rv$dtab[, col_indx] <- plyr::mapvalues(dat_rv$dtab[, col_indx], from = c(input$flevels), to = c(new_names))
    } else if ((length(input$flevels) > 1) && (length(new_names) == 1)) { # if more then one factor selected to rename and only one provided
      new_names <- rep(new_names, length(input$flevels))
      dat_rv$dtab[, col_indx] <- plyr::mapvalues(dat_rv$dtab[, col_indx], from = c(input$flevels), to = c(new_names))
    } else if ((length(input$flevels) > 1) && (length(new_names) > 1)) { # if more then one factor and more then one name provided
      if (length(input$flevels) != length(new_names)) { # if the length don't match
        NULL
      } else {
        dats <- data.frame(dat_rv$dtab)
        dats[, col_indx] <- plyr::mapvalues(dats[, col_indx], from = c(input$flevels), to = c(new_names))
        dat_rv$dtab <- dats
      }
    }
    dat_rv$dtab
  })

  # main function to check possible conversion methods
  dat_neg_conv <- eventReactive(input$accept_negconv_bt, {
    if (nchar(input$neg_text) > 0) {
      expr <- tryCatch({
        check_Coma <- grep(",", input$neg_text, fixed = TRUE)
        if (length(check_Coma) == 0) {
          return()
        }

        s <- strsplit(input$neg_text, ",")[[1]]

        if (length(s) < 2) {
          return()
        }
        else if (grepl("x", (s[2])) == FALSE) {
          return()
        }

        neg_glob <<- input$neg_text
        if (length(input$negts_par) == 1) {
          x <- dat_rv$dtab[, input$negts_par]
          dat_origin <- x

          formulas <- strsplit(neg_glob, ",")

          convert_ev <- eval(parse(text = str_trim(formulas[[1]][1])))
          if (is.null(convert_ev)) {
            return()
          }

          x <- convert_ev
          inverse <- eval(parse(text = str_trim(formulas[[1]][2])))
          inverse <- round(inverse, 3)
          dat_origin <- round(dat_origin, 3)

          if (all(convert_ev >= 0) && (inverse == dat_origin)) {
            return(1)
          }
        } else if (length(input$negts_par) > 1) {
          if (length(str_trim(unlist(strsplit(neg_glob, ",")))) > 2) {
            formulas <- unlist(strsplit(neg_glob, ";"))
            if (length(formulas) == length(input$negts_par)) {
              l <- sapply(formulas, strsplit, split = ",")
              flag <- 0
              for (i in seq(1, length(l))) {
                x <- dat_rv$dtab[, input$negts_par[i]]
                dat_origin <- x
                convert_ev <- eval(parse(text = str_trim(l[[i]][1])))
                x <- convert_ev
                inverse <- eval(parse(text = str_trim(l[[i]][2])))
                inverse <- round(inverse, 3)
                dat_origin <- round(dat_origin, 3)

                if (all(convert_ev >= 0) && (inverse == dat_origin)) {
                  flag <- flag + 1
                }
              }

              if (flag == length(input$negts_par)) {
                return(1)
              }
            } else {
              NULL
            }
          }
        }
      }, error = function(e) {
        return(NULL)
      })
    } else {
      return()
    }
  })

  output$ibox <- renderInfoBox({
    req (datas())
    if (!is.null(dat_neg_conv())) {
      if (dat_neg_conv() == 1) {
        infoBox(
          "OK",
          NULL,
          icon = icon("refresh"),
          width = 4,
          color = "green"
        )
      } else {
        NULL
      }
    } else {
      infoBox(
        "Fail",
        NULL,
        icon = icon("refresh"),
        width = 4,
        color = "red"
      )
    }
  })

  output$ibox_zero <- renderInfoBox({
    req (datas())
    if (!is.null(zero())) {
      infoBox(
        "OK",
        NULL,
        icon = icon("refresh"),
        width = 4,
        color = "green"
      )
    }
  })

  zero <- eventReactive(input$accept_zeroconv_bt, {
    input$zero_const
  })

  # mainfunction to deal with zero
  dat_zero_conv <- eventReactive(input$accept_zeroconv_bt, {
    req(dat_rv$dtab)
  })

  # main function to deal with missing data , impute nondetects using detection limits or other available options (mean, median)
  dat_miss_conv <- eventReactive(input$accept_missconv_bt, {
    req(input$missing_lvl)
    req(input$missing_adj)

    col_ind <- 2 # which(names(dat_rv$dtab) %in% input$facts2)
    dats <- dat_rv$dtab[which(as.character(dat_rv$dtab[, col_ind]) %in% as.character(input$missing_lvl)), ]
    join_flag <- grep("JOIN", input$missing_lvl)

    d <- NULL
    if (length(join_flag) < 1) { # get the mean or median for a subset factors
      for (i in seq(1, length(input$missing_lvl))) {
        dats_sub <- dats[which(as.character(dats[, col_ind]) %in% as.character(input$missing_lvl[i])), ]
        if (nrow(dats_sub) == 1) {
          return(NULL)
        }
        for (j in seq(1, length(input$missing))) {
          if (input$missing_adj == "MEAN") {
            dats_sub[is.na(dats_sub[, input$missing[j]]), input$missing[j]] <- mean(dats_sub[, input$missing[j]], na.rm = TRUE)
          } else if (input$missing_adj == "MEDIAN") {
            dats_sub[is.na(dats_sub[, input$missing[j]]), input$missing[j]] <- median(dats_sub[, input$missing[j]], na.rm = TRUE)
          }
        }
        d <- rbind(d, dats_sub)
      }
      dat_rv$dtab[which(as.character(dat_rv$dtab[, col_ind]) %in% as.character(input$missing_lvl)), ] <- d
      dat_rv$dtab
    } else { # if the join flag tagged, use all of them to get the mean or median from the entire data set

      if (as.numeric(dat_sum()[, input$missing][2]) == length(input$missing_lvl) - 1) {
        ## cat("Not enough observations to fill with mean/median with join \n", file = log_con, append = TRUE)
        return(NULL)
      }
      for (j in seq(1, length(input$missing))) {
        if (input$missing_adj == "MEAN") {
          dats[is.na(dats[, input$missing[j]]), input$missing[j]] <- mean(dats[, input$missing[j]], na.rm = TRUE)
        }
        if (input$missing_adj == "MEDIAN") {
          dats[is.na(dats[, input$missing[j]]), input$missing[j]] <- median(dats[, input$missing[j]], na.rm = TRUE)
        }
      }
      dat_rv$dtab[which(as.character(dat_rv$dtab[, col_ind]) %in% as.character(input$missing_lvl)), ] <- dats
      dat_rv$dtab
    }


    #### CURRENTLY WORKING ON####
    if (input$missing_adj == "Impute" && input$impute_selected == "def") {
      det_lim_vals <- as.numeric(unlist(strsplit(input$det_lim, ",")))

      if (any(is.na(det_lim_vals))) {
        # cat("Ensure that valid numerics and sep. provided \n", file = log_con, append = TRUE)
        return(NULL)
      }

      dat <- (dat_rv$dtab)[, -c(1, 2)] # remove first two columns (source and organic content)
      dat <- dat[, which(!names(dat) %in% input$missing_neg)]
      dl <- rep(0, length(names(dat)))
      ind <- which(names(dat) %in% input$missing)
      dl[ind] <- det_lim_vals

      if (is.na(det_lim_vals)) {
        # cat("Ensure that number of detection limits provided match source data in user default \n", file = log_con, append = TRUE)
        return(NULL)
      }

      dat[is.na(dat)] <- 0
      dats <- lrEM(dat, label = 0, dl = dl, ini.cov = "multRepl")
      dat_rv$dtab[, c(input$missing)] <- dats[, c(input$missing)]
      dat_rv$dtab
    } else if (input$missing_adj == "Impute" && input$impute_selected == "sel_mat") {
      req(filedata())
      dat <- filedata()
      src <- dat_rv$dtab
      src_mat <- src[, na.omit(match(colnames(src), colnames(dat)))]
      dl_mat <- dat
      src_mat <- src_mat[, which(!colnames(src_mat) %in% input$missing_neg)]
      dl_mat <- dl_mat[, which(!colnames(dl_mat) %in% input$missing_neg)]
      src_mat[is.na(src_mat)] <- 0
      src_mat <- data.matrix(src_mat[, -c(1, 2)])
      dl_mat <- data.matrix(dl_mat[, -c(1, 2)])
      dl_mat <- dl_mat[, which(colnames(dl_mat) %in% colnames(src_mat))]
      src_mat <- src_mat[, which(colnames(src_mat) %in% colnames(dl_mat))]

      if (dim(src_mat) != dim(dl_mat)) {
        # cat("Ensure that matrix dimensions match \n", file = log_con, append = TRUE)
        return(NULL)
      }

      impute_dat <- lrEM(src_mat, label = 0, dl = dl_mat, ini.cov = "multRepl")
      dat_rv$dtab[, c(input$missing)] <- impute_dat[, c(input$missing)]
      dat_rv$dtab
    } else if (input$missing_adj == "Impute" && input$impute_selected == "sel_tab") {
      req(filedata())
      dats <- filedata()
      det_lim_vals <- dats[, 2][(match(input$missing, dats[, 1]))]
      dat <- (dat_rv$dtab)[, -c(1, 2)] # remove first two columns (source and organic content)
      dat <- dat[, which(!names(dat) %in% input$missing_neg)]
      dl <- rep(0, length(names(dat)))
      ind <- which(names(dat) %in% input$missing)
      dl[ind] <- det_lim_vals
      dat[is.na(dat)] <- 0


      if (is.na(det_lim_vals)) {
        return(NULL)
      }

      dats <- lrEM(dat, label = 0, dl = dl, ini.cov = "multRepl")
      dat_rv$dtab[, c(input$missing)] <- dats[, c(input$missing)]
      dat_rv$dtab
    }
  })
  
  observeEvent(input$accept_revalue_bt, {
    output$dat_levels_dt <- renderDT ({
      dat_levels_rename()
      dat_rv$dtab
    })
  })
  
  output$dat_levels_dt <- renderDT({
      dat_rv$dtab
  })

  observeEvent(input$accept_negconv_bt, {
    dat_neg_conv()
  })

  observeEvent(input$accept_zeroconv_bt, {
    zero_const <<- input$zero_const
  })

  observeEvent(input$accept_missconv_bt, {
    dat_miss_conv()
  })

  rownames_missing <- reactive({
    dats <- dat_rv$dtab[, c(input$missing)]
    dat_rv$dtab[as.vector(attributes(na.omit(dats))$na.action), ]
  })

  datOutput <- reactive({
    req (dat_rv$dtab)
    dat <- dat_rv$dtab
    dat <- dat[order(dat[, 2]), ]
    dat
  })

  return(datOutput)
}
