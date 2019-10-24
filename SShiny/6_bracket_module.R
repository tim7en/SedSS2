#source ("func.R")

bracket_apply<- function (x,y, r) {
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
      l[[i]] <- cbind (paste0('Target',rownames(yNum)[which(yNum[,i] %in% dat)]), colnames(yNum)[i])
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
            uiOutput(ns("bracketRange"))
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
                  withSpinner (DTOutput(ns("targetBracketOutput"))),
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
                  withSpinner (DTOutput(ns("targetDroplistOutput"))),
                  style = "height:auto; overflow-y: scroll;overflow-x: scroll;"
                )
              ),
              br (),
              br (),
              downloadButton (ns('downloadRemoved'), 'Download')
            )
          ) 
        )
      )
    )
  )
}

bracket_server <- function (input, output, session, data, target){
  
  output$downloadRemoved <- downloadHandler(
    filename = function() {
      paste('Failed_bracket', input, Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(  targetremovedfunction(), file, row.names = F)
    }
  )

  output$bracketRange <- renderUI({
    ns <- session$ns
    numericInput(ns("bracketRngInput"), "Bracket range parameter", value = 0.1, min = 0, max = 1, step = 0.005)
  })
  
  targetsbracketsfunction <- reactive({
    req(data())
    req(input$bracketRngInput)
    x <- data()[[1]]
    y <- target() # list of targets corrected
    y <- as.data.frame(y)
    dat <- NULL
    trg <- NULL
    
    for (i in seq(1, nrow(y))) {
      l <- bracket_apply(x[[i]], y[i, ], input$bracketRngInput) #bracket function
      dat <- rbind(dat, l[[1]])

      if (nrow(l[[2]])>0){
        l[[2]][,1] <- as.character(y[i,1])
      }
      trg <- rbind(trg, l[[2]])
    }
    dat <- data.frame(dat)
    trg <- data.frame(trg)
    list(dat, trg)
  })

  targetremovedfunction <- reactive({
    req(targetsbracketsfunction())
    dat <- targetsbracketsfunction ()[[2]] #empty data frame
  })
  
  output$targetDroplistOutput <- renderDT({
    dat <- targetremovedfunction()
    names (dat) <- c('Target', 'Element')
    dat
  })
  
  output$targetBracketOutput <- renderDT({
    req(targetsbracketsfunction())
    dat <- targetsbracketsfunction()
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
  
  return (targetsbracketsfunction)
  
}