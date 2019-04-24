#source ("func.R")

bracketT<- function (x,y, r) {
  d <- y
  y <- y[,-c(1,2)]
  x <- x[, 3:ncol(x)]
  x <- apply(x, 2, as.numeric)
  x <- as.data.frame(x)

  colSrc <- match(colnames(x), colnames(y))
  
  yNum <- data.frame(y[, na.omit(colSrc)])
  upperL <- x * (1 + r)
  lowerL <- x * (1 - r)

  d <- NULL
  l <- list ()
  for (i in seq (1, ncol(upperL))){
    datmax <- subset(yNum[,i], abs(yNum[,i])>max(abs(upperL[,i])))
    datmin <- subset(yNum[,i], abs(yNum[,i])<min(abs(lowerL[,i])))
    dat <- c(datmax, datmin)
    
    if (length(dat)>0){
      l[[i]] <- cbind (paste0('target',rownames(yNum)[which(yNum[,i] ==dat)]), colnames(yNum)[i])
    } else {
      l[[i]] <- cbind (NA,NA)
    }
    yNum[which (yNum[,i] == dat),i] <- paste0(yNum[which (yNum[,i] == dat),i], "*", sep = "")
    d <- rbind (d, l[[i]])
    
  }
  d <- na.omit (d)
  list(yNum, d)
}




bracket_ui <- function (id){
  
  ns <- NS(id)
  tagList(
    fluidPage(
      sidebarLayout(
        div(
          style = "width: 50%;",
          sidebarPanel(
            uiOutput(ns("brack_range"))
          ), width = 2
        ),
        mainPanel(
          width = 10,
          tabsetPanel(
            tabPanel(
              'Targets',
              box(
                title = "Target Brackets", status = "success", height =
                  "595", width = "12", solidHeader = T,
                column(
                  width = 12,
                  withSpinner (DTOutput(ns("targets_brackets_output"))),
                  style = "height:auto; overflow-y: scroll;overflow-x: scroll;"
                )
              )
            ),
            tabPanel(
              'Drops',
              box(
                title = "Target Drop", status = "success", height =
                  "595", width = "12", solidHeader = T,
                column(
                  width = 12,
                  withSpinner (DTOutput(ns("target_droplist_output"))),
                  style = "height:auto; overflow-y: scroll;overflow-x: scroll;"
                )
              )
            )
          ) 
        )
      )
    )
  )
}

bracket_server <- function (input, output, session, data, target){
  
  output$brack_range <- renderUI({
    ns <- session$ns
    sliderInput(ns("brack_rng"), "Bracket range parameter", value = 0.1, min = 0, max = 1, step = 0.05)
  })
  
  targets_brackets_function <- reactive({
    req(data())
    req(input$brack_rng)
    x <- data()[[1]] # list of sources corrected
    y <- target() # list of targets corrected
    y <- as.data.frame(y)
    dat <- NULL
    trg <- NULL
    
    x_glob <<- x
    y_glob<<-y
    brack_glob <<- input$brack_rng
    for (i in seq(1, nrow(y))) {
      l <- bracketT(x[[i]], y[i, ], input$brack_rng) #bracket function
      dat <- rbind(dat, l[[1]])
      trg <- rbind(trg, l[[2]])
    }
    dat <- data.frame(dat)
    trg <- data.frame(trg)
    list(dat, trg)
  })

  target_removed_function <- reactive({
    req(targets_brackets_function())
    dat <- targets_brackets_function ()[[2]] #empty data frame
  })
  
  output$target_droplist_output <- renderDT({
    dat <- target_removed_function()
    names (dat) <- c('Target', 'Element')
    dat
  })
  
  output$targets_brackets_output <- renderDT({
    req(targets_brackets_function())
    dat <- targets_brackets_function()
    grepF <-function (x){
      return (x[grep('*', x, fixed = TRUE)])
    }
    selection <- array (unlist (apply(dat[[1]], 2, grepF)))
    if (length(selection>0)){
      DT::datatable(dat[[1]]) %>% formatStyle(
        c(colnames(dat[[1]])),
        backgroundColor = styleEqual(selection, rep("lightsalmon", length(selection)))
      )
    } else {
      DT::datatable(dat[[1]])
    }
  })
  
  return (targets_brackets_function)
  
}