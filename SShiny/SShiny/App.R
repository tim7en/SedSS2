rm(list = setdiff(ls(), ""))
source("pcg.R")
source("1_upload_module.R")
source("2_datacheck_module.R")
source("3_normalize_module.R")
source("4_outliers_module.R")
source("5_correction_module.R")
source("6_bracket_module.R")
source("7_dfa_module.R")
source("8_mixing_module.R")
source('convMixed.R')


server <- function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })

  
  # the modal dialog where the user can enter the query details.
  query_modal <- modalDialog(
    title = "Welcome!",
    "Welcome to the first version of sediment fingerprinting tool built in Shiny framework.
    Plase watch this short youtube video or close to advance.",
    easyClose = F,
    footer = tagList(
      actionButton("watch", "Watch Tutorial"),
      actionButton('watch_skip', 'Close')
    )
  )
  # Show the model on start up ...
  showModal(query_modal)
  
  # function to create a modal, can be different functions as well
  makeModal <- function(text, id){
    datid <-eval(parse (text = paste0('input$',id))) 
    if (is.null(datid) || isFALSE(datid)){
      modalDialog(
        footer=list(modalButton("Close"), checkboxInput(id, 'Prevent window from opening')),
        p(text)
      )
    }
  }
  
  observeEvent(input$sbmenu, {
    if (input$sbmenu == "DatCheck"){
      
      showModal(makeModal("This block of a dashboard used to rename source if needed, impute nondetects,
          provide right methods to convert negative values into positive or add a constant to zero values.",
                          'datcheck'))

    } else if (input$sbmenu == "Transformations"){
      
      showModal(makeModal("Current part of a dashboard used to review ladder of power of transformations,
                          before outliers detection. Ladder of power defines optimal transformation used
                          to normalize data and assess outliers outside of defined range from the central tendency (mean).",
                          'transfrm'))
      
    } else if (input$sbmenu == "Outliers"){

      showModal(makeModal("This part of the panel review  previously found outliers and allows user to edit source table.",
                          'outliers'))
    
    } else if (input$sbmenu == "Correction"){

      showModal(makeModal("This block of a dashboard used to correct source data for size, organic or both.",
                          'correction'))
      
    } else if (input$sbmenu == "Bracket"){
      
      showModal(makeModal("Current part removes elements which are outside of the source range.",
                          'bracket'))
      
    } else if (input$sbmenu == "DFA"){
      
      showModal(makeModal("Discriminant function used to differentiate source data before running a mixing model.",
                          'dfa'))
      
    } else if (input$sbmenu == "mixmod"){
      
      showModal(makeModal("Used to run a mixing model, analyze errors with a cross validation.",
                          'mix'))
      
    }
    
  })
  
  observeEvent(input$watch_skip, {
    removeModal()
  })
  
  observeEvent(input$watch, {
    browseURL("https://drive.google.com/file/d/16_iSTwLFRjLduxci_CTkbS9GEDmphYQW/view?usp=sharing", browser = getOption("browser"),
              encodeIfNeeded = FALSE)
  })
  
    
  zero_const <<- 0.001
  neg_glob <<- 'x+1,x-1'
  
  
  source_input <- callModule(upload_server, "source_upload")
  target_input <- callModule(upload_server, "target_upload")
  dat_check <- callModule (sidebar_func, "source_check", source_input)
  normalize <- callModule(normalize_server, "transform", dat_check)
  outliers <- callModule(outliers_server, "outliers", dat_check, normalize)
  correction <- callModule (correction_server, "correct", outliers, target_input) #corrected sourceinput list
  bracket <- callModule (bracket_server, "bracket_f", correction, target_input) #target input with drop 
  dfa <- callModule (dfa_server, "dfa_f", correction, bracket)
  mixing <- callModule (mixing_server, "mixing_m", correction, target_input, dfa)
}

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "SShiny"),
  dashboardSidebar(
    sidebarMenu( id = "sbmenu",
      menuItem("INPUT", tabName = "dataInput", icon = icon("upload")),
      menuItem("DATA CHECK", tabName = "DatCheck", icon = icon('calendar-check')),
      menuItem("TRANSFORMATION", tabName = "Transformations", icon = icon("refresh")),
      menuItem("OUTLIERS", tabName = "Outliers", icon = icon("remove")),
      menuItem("SIZE & TOC CORRECTION", tabName = "Correction", icon = icon("random")),
      menuItem("BRACKET TEST", tabName = "Bracket", icon = icon("braille")),
      menuItem("DISCRIMINANT FUNCTION", tabName = "DFA", icon = icon("tasks")),
      menuItem("MIXING MODEL", tabName = "mixmod", icon = icon("rocket"))
    )
  )
  ,
  dashboardBody(
    tabItems(
      tabItem( # First tab content
        tabName = "dataInput",
        tabsetPanel(
          tabPanel(
            "Source",
            upload_ui ("source_upload")
          )
          ,
          tabPanel(
            "Target",
            upload_ui ("target_upload")
          )
        )
      )
      ,
      tabItem (
        tabName = "DatCheck",
        tabPanel (
          "Data check",
          sidebarLayout(
            sidebarPanel(
              my_sidebar ("source_check")
            ),
            mainPanel (
              my_mainpage ("source_check")
            )
          )
        )
       )
      ,
      tabItem (
        tabName = "Transformations",
        normalize_ui ("transform")
      )
      ,
      tabItem (
        tabName = "Outliers",
        outliers_ui ("outliers")
      )
      ,
      tabItem(
        tabName = "Correction",
        correction_ui ("correct")
      )
      ,
      tabItem(
        tabName = "Bracket",
        bracket_ui ("bracket_f")
      )
      ,
      tabItem(
        tabName = "DFA",
        dfa_ui ("dfa_f")
      )
      ,
      tabItem(
        tabName = "mixmod",
        mixing_ui ("mixing_m")
      )
    )
  )
)

shinyApp(ui, server)