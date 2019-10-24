multivariate_ui <- function (id){
        ns <- NS(id)
        tagList(
          fluidPage(
            fluidRow(
        
        sidebarLayout(
          sidebarPanel(
            uiOutput (ns('selectTarget'))
          ),
          mainPanel (
            DTOutput(ns ('myoutput'))
          )
        )
      )
    )
  )
}

multivariate_server <- function (input, output, session, corrected_function, target_function, target_input) {
  #clean list of source files and remove one from the bracket
  source_l_drop <- reactive ({
    l_src <- corrected_function ()[[1]] #source list data frames
    drops <- target_function ()[[2]]
    target <- target_input ()

    if (length (as.character(unique (drops[,1]))) >0){
      for (j in seq (1, nrow (drops))){
        indx <- which(target[,1] %in% drops[j,1])
        l_src[[indx]] <- l_src[[indx]][,!(names(l_src[[indx]]) %in% drops[j,2])]
      }
      return (l_src)
    } else {
      return (l_src) #list of source data
    }
    })
  
  #list of source data
  myoutput <- reactive ({
    source_l_drop <- lapply (source_l_drop(), OUTPUT_MVTEST)
    source_l_drop
  })
  
  output$selectTarget <- renderUI({
    ns <- session$ns
    selectInput(ns("selectedTarget"), 'Select source to show', choices = target_input()[,1], multiple = FALSE, selected=target_input()[1,1], width = "100%")
  })
  
  output$myoutput <- renderDT ({
    indx <- which (target_function()[[1]][,1] %in% input$selectedTarget)
    myoutput[[indx]]
  })
  
  myoutput
}
  