# lower camelcase mode used for UI elements output (input)
# Lower case with underscore used for functions
# All lower case used for reactive functions only

options(shiny.fullstacktrace = TRUE)
# remove warning
suppressWarnings({
  suppressPackageStartupMessages({
    source("check_types.R")
    source("2_datacheck_functions.R")
    library(shiny)
    library(DT)
    library(plyr)
    library(shinydashboard)
    library(zCompositions)
  })
})

# app sidebar
my_sidebar <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(
        "RENAME",
        "This page assists in checking and renaming sediment sources",
        br(),
        br(),
        uiOutput(ns("datLevels")),
        uiOutput(ns("newLevels")),
        uiOutput(ns("acceptRevalue"))
      ),
      tabPanel(
        "Negatives",
        uiOutput(ns("datNegativesPar")),
        uiOutput(ns("datNegativesText")),
        uiOutput(ns("acceptNegConv")),
        br(),
        fluidRow(
          column(
            width = 6,
            infoBoxOutput(ns("iBox")),
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
        uiOutput(ns("datMissing")),
        uiOutput(ns("datDetLimit")),
        uiOutput(ns("datMissingSub")),
        uiOutput(ns("datMissingAdj")),
        uiOutput(ns("datMissNegSel")),
        uiOutput(ns("imputeMissing")),
        uiOutput(ns("acceptMissConv"))
      ),
      tabPanel(
        "Zero",
        uiOutput(ns("datMissZeroSelect")),
        uiOutput(ns("zeroConstant")),
        uiOutput(ns("acceptZeroConversion")),
        br(),
        fluidRow(
          column(
            width = 6,
            infoBoxOutput(ns("iBoxZero")),
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
          withSpinner(DTOutput(ns("datLevelsDtable"))),
          style = "height:'auto'; overflow-y: scroll; overflow-x: scroll;"
        ),
        br(),
        br(),
        downloadButton(ns("downloadDatas"), "Download"),
        uiOutput(ns("datLimUi"))
      ),
      tabPanel(
        "Summary",
        box(
          title = textOutput(ns("datNrow")), status = "danger", height = "630", width = 12, solidHeader = T,
          withSpinner(DTOutput(ns("distTable"))),
          style = "height:'auto'; overflow-y: scroll;overflow-x: scroll"
        ),
        br(),
        br(),
        downloadButton(ns("downloadSummary"), "Download")
      )
    )
  )
}

sidebar_func <- function(input, output, session, datas, tdatas) {
  ns <- session$ns
  datrv <- reactiveValues(dataTable = NULL)

  output$downloadDatas <- downloadHandler(
    filename = function() {
      paste("Data_check_source-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datoutput(), file, row.names = F)
    }
  )

  output$downloadSummary <- downloadHandler(
    filename = function() {
      paste("Data_check_summary-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data.frame(datsum()), file, row.names = F)
    }
  )

  observe({
    req(datas())
    datrv$dataTable <- datas()
  })

  output$datNrow <- renderText({
    paste0("Summary table of data with rows/columns: ", paste(nrow(datrv$dataTable), ncol(datrv$dataTable), sep = "/"))
  })

  output$distTable <- renderDT({
    data.frame(datsum())
  })

  datsum <- reactive({
    req(datrv$dataTable)
    data_summary(datrv$dataTable)
  })

  output$datFacts <- renderUI({
    ns <- session$ns
    req(datas())
    dat <- datsum()
    s <- colnames(dat)[which(as.numeric(as.matrix(dat[3, ])) > 0)]
    selectInput(ns("factorsData"), "Available sources", choices = c(s[2]), selected = c(s[2]))
  })

  output$datNegativesPar <- renderUI({
    ns <- session$ns
    dat <- datsum()
    s <- colnames(dat)[which(as.numeric(as.matrix(dat[4, ])) > 0 & as.numeric(as.matrix(dat[4, ])) < nrow(datrv$dataTable))]
    selectInput(ns("negtsPar"), "Some values below 0", choices = c(s), selected = c(s), multiple = TRUE)
  })

  output$datNegativesText <- renderUI({
    ns <- session$ns
    textInput(ns("negText"), "Method, default is (x/1000 +1)", value = "x/1000+1, (x-1)*1000")
  })

  output$datMissing <- renderUI({
    ns <- session$ns
    dat <- datsum()
    s <- colnames(dat)[which(as.numeric(as.matrix(dat[2, ])) > 0)]
    selectInput(ns("missingDat"), "Values are missing", choices = c(s), selected = c(s), multiple = TRUE)
  })

  output$datMissingSub <- renderUI({
    ns <- session$ns
    sel_names <- rownamesmissingdata()[, 2]
    selectInput(ns("missingDatLvl"), "Sources", choices = c(levels(datrv$dataTable[, 2])), multiple = TRUE, selected = sel_names) # one that are missingDat values
  })

  output$datMissingAdj <- renderUI({
    ns <- session$ns
    selectInput(ns("missingDatAdj"), "Option", choices = c("Impute"), selected = NULL, multiple = FALSE)
  })

  # could be replaced to textOutput instead of selectInput
  output$datMissNegSel <- renderUI({
    req(input$missingDatAdj)
    ns <- session$ns
    dat <- datsum()
    cl_remove <- unique(c(colnames(dat)[which(as.numeric(as.matrix(dat[4, ])) > 0)], colnames(dat)[which(as.numeric(as.matrix(dat[5, ])) > 0)]))
    if (input$missingDatAdj == "Impute") {
      clnames <- colnames(dat)[which(as.numeric(as.matrix(dat[2, ])) > 0)]
      drops <- which(clnames %in% input$missingDat)
      clnames <- clnames[-c(drops)]
      mychoices <- colnames(dat)[which(!colnames(dat) %in% c(input$missingDat))]
      mychoices <- (mychoices[-c(1, 2)])
      selectInput(ns("missingDatNeg"), "Excluded from imputation (negatives, zero)", choices = c(mychoices, clnames), selected = c(cl_remove, clnames), multiple = TRUE)
    } else {
      NULL
    }
  })

  output$datDetLimit <- renderUI({
    req(input$missingDatAdj)
    req(input$imputeSelected)
    ns <- session$ns
    if (input$imputeSelected == "def") {
      if (input$missingDatAdj == "Impute") {
        textInput(ns("detLim"), "Detection limits", value = "1")
      } else {
        NULL
      }
    } else { # This function is repsonsible for loading in the selected file
      fileInput(ns("dataFile"), "Choose CSV file",
        accept = c("text/csv", "text/comma-separated-values,text/plain")
      )
    }
  })

  # could be replaced to textOutput instead of selectInput
  output$datMissZeroSelect <- renderUI({
    ns <- session$ns
    dat <- datsum()
    zero <- unique(c(colnames(dat)[which(as.numeric(as.matrix(dat[5, ])) > 0)], colnames(dat)[which(as.numeric(as.matrix(dat[5, ])) > 0)]))
    selectInput(ns("missingDatZero"), "Zero found: ", choices = zero, selected = c(zero), multiple = TRUE)
  })

  output$zeroConstant <- renderUI({
    ns <- session$ns
    numericInput(ns("zeroConstant"), "Constant", 0.001, 0.001)
  })

  # This function is repsonsible for loading in the selected file
  filedata <- reactive({
    infile <- input$dataFile
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv(infile$datapath)
  })

  output$dtLimits <- renderDT({
    req(input$imputeSelected)
    if (input$imputeSelected == "def") {
      return(NULL)
    }
    else {
      filedata()
    }
  })

  output$datLimUi <- renderUI({
    ns <- session$ns
    req(input$imputeSelected != "def")
    box(
      width = 12,
      title = "Detection limit", solidHeader = TRUE, status = "primary",
      DTOutput(ns("dtLimits")),
      style = "height:'auto'; overflow-y: scroll;overflow-x: scroll;"
    )
  })

  output$datLevels <- renderUI({
    ns <- session$ns
    selectInput(ns("factLevels"), "Available sources", choices = levels(datrv$dataTable[, 2]), multiple = TRUE, selected = FALSE)
  })

  output$newLevels <- renderUI({
    ns <- session$ns
    req(datas())
    textInput(ns("newNames"), "Rename")
  })

  output$acceptRevalue <- renderUI({
    ns <- session$ns
    actionButton(ns("acceptRevalueButton"), "Accept")
  })

  output$acceptNegConv <- renderUI({
    ns <- session$ns
    actionButton(ns("acceptNegConvButton"), "Accept")
  })

  output$acceptZeroConversion <- renderUI({
    ns <- session$ns
    actionButton(ns("acceptZeroConversionButton"), "Accept")
  })

  output$acceptMissConv <- renderUI({
    ns <- session$ns
    actionButton(ns("acceptMissConvButton"), "Accept")
  })

  output$imputeMissing <- renderUI({
    ns <- session$ns
    req(input$missingDatAdj)
    if (input$missingDatAdj == "Impute") {
      radioButtons(
        ns("imputeSelected"), "Impute method :",
        c(
          "User type" = "def",
          "Upload matrix" = "selMat",
          "Upload table" = "selTab"
        )
      )
    } else {
      NULL
    }
  })

  # main function to rename factor levels
  datLevels_rename <- eventReactive(input$acceptRevalueButton, {
    col_indx <- 2 # which(names(datrv$dataTable) == input$factorsData) # find columns selected
    newNames <- try(unlist(strsplit(input$newNames, ","))) # unlist user input and use coma as separator
    if ((length(input$factLevels) == 1) && (length(newNames) == 1)) { # if only one factor selected to rename and only one provided
      datrv$dataTable[, col_indx] <- plyr::mapvalues(datrv$dataTable[, col_indx], from = c(input$factLevels), to = c(newNames))
      datrv$dataTable[, col_indx] <- factor(datrv$dataTable[, col_indx], levels = c(as.character(unique(datrv$dataTable[, col_indx]))))
    } else if ((length(input$factLevels) > 1) && (length(newNames) == 1)) { # if more then one factor selected to rename and only one provided
      newNames <- rep(newNames, length(input$factLevels))
      datrv$dataTable[, col_indx] <- plyr::mapvalues(datrv$dataTable[, col_indx], from = c(input$factLevels), to = c(newNames))
      datrv$dataTable[, col_indx] <- factor(datrv$dataTable[, col_indx], levels = c(as.character(unique(datrv$dataTable[, col_indx]))))
    } else if ((length(input$factLevels) > 1) && (length(newNames) > 1)) { # if more then one factor and more then one name provided
      if (length(input$factLevels) != length(newNames)) { # if the length don't match
        NULL
      } else {
        dats <- data.frame(datrv$dataTable)
        dats[, col_indx] <- plyr::mapvalues(dats[, col_indx], from = c(input$factLevels), to = c(newNames))
        datrv$dataTable[, col_indx] <- factor(datrv$dataTable[, col_indx], levels = c(as.character(unique(datrv$dataTable[, col_indx]))))
        datrv$dataTable <- dats
      }
    }
    datrv$dataTable
  })

  # main function to check possible conversion methods
  datnegconv <- eventReactive(input$acceptNegConvButton, {
    if (nchar(input$negText) > 0) {
      expr <- tryCatch(
        {
          check_Coma <- grep(",", input$negText, fixed = TRUE)
          if (length(check_Coma) == 0) {
            return()
          }
          s <- strsplit(input$negText, ",")[[1]]
          if (length(s) < 2) {
            return()
          }
          else if (grepl("x", (s[2])) == FALSE) {
            return()
          }
          negGlob <<- input$negText
          if (length(input$negtsPar) == 1) {
            x <- datrv$dataTable[, input$negtsPar]
            x <- rbind(tdatas()[, input$negtsPar])
            dat_origin <- x
            formulas <- strsplit(negGlob, ",")
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
          } else if (length(input$negtsPar) > 1) {
            if (length(str_trim(unlist(strsplit(negGlob, ",")))) > 2) {
              formulas <- unlist(strsplit(negGlob, ";"))
              if (length(formulas) == length(input$negtsPar)) {
                l <- sapply(formulas, strsplit, split = ",")
                flag <- 0
                for (i in seq(1, length(l))) {
                  x <- datrv$dataTable[, input$negtsPar[i]]
                  x <- rbind(tdatas()[, input$negtsPar[i]])
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
                if (flag == length(input$negtsPar)) {
                  return(1)
                }
              } else {
                NULL
              }
            }
          }
        },
        error = function(e) {
          return(NULL)
        }
      )
    } else {
      return()
    }
  })

  output$iBox <- renderInfoBox({
    req(datas())
    if (!is.null(datnegconv())) {
      if (datnegconv() == 1) {
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

  output$iBoxZero <- renderInfoBox({
    req(datas())
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

  zero <- eventReactive(input$acceptZeroConversionButton, {
    input$zeroConstant
  })

  # mainfunction to deal with zero
  datzeroconv <- eventReactive(input$acceptZeroConversionButton, {
    req(datrv$dataTable)
  })

  # main function to deal with missingDat data , impute nondetects using detection limits or other available options (mean, median)
  datmissingconv <- eventReactive(input$acceptMissConvButton, {
    req(input$missingDatLvl)
    req(input$missingDatAdj)

    col_ind <- 2 # which(names(datrv$dataTable) %in% input$factorsData2)
    dats <- datrv$dataTable[which(as.character(datrv$dataTable[, col_ind]) %in% as.character(input$missingDatLvl)), ]
    join_flag <- grep("JOIN", input$missingDatLvl)

    d <- NULL
    if (length(join_flag) < 1) { # get the mean or median for a subset factors
      for (i in seq(1, length(input$missingDatLvl))) {
        dats_sub <- dats[which(as.character(dats[, col_ind]) %in% as.character(input$missingDatLvl[i])), ]
        if (nrow(dats_sub) == 1) {
          return(NULL)
        }
        for (j in seq(1, length(input$missingDat))) {
          if (input$missingDatAdj == "MEAN") {
            dats_sub[is.na(dats_sub[, input$missingDat[j]]), input$missingDat[j]] <- mean(dats_sub[, input$missingDat[j]], na.rm = TRUE)
          } else if (input$missingDatAdj == "MEDIAN") {
            dats_sub[is.na(dats_sub[, input$missingDat[j]]), input$missingDat[j]] <- median(dats_sub[, input$missingDat[j]], na.rm = TRUE)
          }
        }
        d <- rbind(d, dats_sub)
      }
      datrv$dataTable[which(as.character(datrv$dataTable[, col_ind]) %in% as.character(input$missingDatLvl)), ] <- d
      datrv$dataTable
    } else { # if the join flag tagged, use all of them to get the mean or median from the entire data set

      if (as.numeric(datsum()[, input$missingDat][2]) == length(input$missingDatLvl) - 1) {
        ## cat("Not enough observations to fill with mean/median with join \n", file = log_con, append = TRUE)
        return(NULL)
      }
      for (j in seq(1, length(input$missingDat))) {
        if (input$missingDatAdj == "MEAN") {
          dats[is.na(dats[, input$missingDat[j]]), input$missingDat[j]] <- mean(dats[, input$missingDat[j]], na.rm = TRUE)
        }
        if (input$missingDatAdj == "MEDIAN") {
          dats[is.na(dats[, input$missingDat[j]]), input$missingDat[j]] <- median(dats[, input$missingDat[j]], na.rm = TRUE)
        }
      }
      datrv$dataTable[which(as.character(datrv$dataTable[, col_ind]) %in% as.character(input$missingDatLvl)), ] <- dats
      datrv$dataTable
    } 
    
    if (input$missingDatAdj == "Impute" && input$imputeSelected == "def") {
      detLim_vals <- as.numeric(unlist(strsplit(input$detLim, ",")))
      if (any(is.na(detLim_vals))) {
        return(NULL)
      }
      dat <- (datrv$dataTable)[, -c(1, 2)] # remove first two columns (source and organic content)
      dat <- dat[, which(!names(dat) %in% input$missingDatNeg)]
      dl <- rep(0, length(names(dat)))
      ind <- which(names(dat) %in% input$missingDat)
      dl[ind] <- detLim_vals
      if (is.na(detLim_vals)) {
        return(NULL)
      }
      dat[is.na(dat)] <- 0
      dats <- lrEM(dat, label = 0, dl = dl, ini.cov = "multRepl")
      datrv$dataTable[, c(input$missingDat)] <- dats[, c(input$missingDat)]
      datrv$dataTable
    } else if (input$missingDatAdj == "Impute" && input$imputeSelected == "selMat") {
      req(filedata())
      dat <- filedata()
      src <- datrv$dataTable
      src_mat <- src[, na.omit(match(colnames(src), colnames(dat)))]
      dl_mat <- dat
      src_mat <- src_mat[, which(!colnames(src_mat) %in% input$missingDatNeg)]
      dl_mat <- dl_mat[, which(!colnames(dl_mat) %in% input$missingDatNeg)]
      src_mat[is.na(src_mat)] <- 0
      src_mat <- data.matrix(src_mat[, -c(1, 2)])
      dl_mat <- data.matrix(dl_mat[, -c(1, 2)])
      dl_mat <- dl_mat[, which(colnames(dl_mat) %in% colnames(src_mat))]
      src_mat <- src_mat[, which(colnames(src_mat) %in% colnames(dl_mat))]
      if (dim(src_mat) != dim(dl_mat)) {
        return(NULL)
      }
      impute_dat <- lrEM(src_mat, label = 0, dl = dl_mat, ini.cov = "multRepl")
      datrv$dataTable[, c(input$missingDat)] <- impute_dat[, c(input$missingDat)]
      datrv$dataTable
    } else if (input$missingDatAdj == "Impute" && input$imputeSelected == "selTab") {
      req(filedata())
      dats <- filedata()
      detLim_vals <- dats[, 2][(match(input$missingDat, dats[, 1]))]
      dat <- (datrv$dataTable)[, -c(1, 2)] # remove first two columns (source and organic content)
      dat <- dat[, which(!names(dat) %in% input$missingDatNeg)]
      dl <- rep(0, length(names(dat)))
      ind <- which(names(dat) %in% input$missingDat)
      dl[ind] <- detLim_vals
      dat[is.na(dat)] <- 0
      if (is.na(detLim_vals)) {
        return(NULL)
      }
      dats <- lrEM(dat, label = 0, dl = dl, ini.cov = "multRepl")
      datrv$dataTable[, c(input$missingDat)] <- dats[, c(input$missingDat)]
      datrv$dataTable
    }
  })

  observeEvent(input$acceptRevalueButton, {
    output$datLevelsDtable <- renderDT({
      datLevels_rename()
      datrv$dataTable
    })
  })

  output$datLevelsDtable <- renderDT({
    datrv$dataTable
  })

  observeEvent(input$acceptNegConvButton, {
    datnegconv()
  })

  observeEvent(input$acceptZeroConversionButton, {
    zeroConstant <<- input$zeroConstant
  })

  observeEvent(input$acceptMissConvButton, {
    datmissingconv()
  })

  rownamesmissingdata <- reactive({
    dats <- datrv$dataTable[, c(input$missingDat)]
    datrv$dataTable[as.vector(attributes(na.omit(dats))$na.action), ]
  })

  datoutput <- reactive({
    req(datrv$dataTable)
    dat <- datrv$dataTable
    dat <- dat[order(dat[, 2]), ]
    dat
  })

  return(datoutput)
}
